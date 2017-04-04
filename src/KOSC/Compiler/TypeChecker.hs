{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-| This module implements type checking functionality. -}
module KOSC.Compiler.TypeChecker where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Foldable
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import           KOSC.Compiler.Common
import           KOSC.Compiler.ScopeChecker
import qualified KOSC.Language.AST            as AST

-- | A type scheme quantifies over some generic variables and carries accessibility information.
data TypeScheme = TypeScheme [AST.Ident] (AST.Type AST.ScopedName) AST.Accessibility

-- | Internal environment of the type checker.
data TypeEnv = TypeEnv
  { _typeMap            :: Map AST.ScopedName TypeScheme
  -- ^ Maps identifiers to their types
  , _requiredReturnType :: Maybe TypeScheme
  -- ^ The required return type when checking a function
  , _globalTypeMap      :: Map AST.ScopedName ScopedTypeExport
  -- ^ Maps type identifiers to their declarations
  }

makeLenses ''TypeEnv

-- | Type checks a module and adds some extra information in the AST.
typeChecker :: Monad m => Map AST.ModuleName ScopedModule -> ScopedModule -> KOSCCompilerT m ScopedModule
typeChecker imports inputMod = evalStateT checkModule initialEnv where
  -- combine all exports of all modules
  initialEnv = TypeEnv
    { _typeMap = Map.empty -- no identifiers declared yet
    , _requiredReturnType = Nothing -- nothing to return on the top level
    , _globalTypeMap = foldOf (folded . scopedModuleTypes) imports -- aggregate information about exported types from all imported modules
    }

  -- the following functions recursively traverse the AST and implement the typing rules
  -- since there is no formal description of the type system yet, there are some ad-hoc
  -- decisions that have been made that probably could be solved in better ways.

  checkModule = do
    -- gather types of globally declared identifiers
    populateGlobalTypeEnv imports
    enterModule (inputMod ^. scopedModuleAST . AST.moduleName) $
      -- check every declaration of the current module
      mapMOf (scopedModuleAST . AST.declarations . traversed) checkDecl inputMod

  checkDecl (AST.DeclFun fdecl) = AST.DeclFun <$> checkFunDecl fdecl
  checkDecl (AST.DeclVar vdecl) = AST.DeclVar <$> checkVarDecl vdecl
  checkDecl d@(AST.DeclBuiltin (AST.BuiltinStruct ssig)) = do
    validateInheritance (AST.makeGlobalName (inputMod ^. scopedModuleAST . AST.moduleName) (ssig ^. AST.structSigName))
    return d
  checkDecl other               = return other

  checkFunDecl (AST.FunDecl sig body) = enterScope $ enterDecl (sig ^. AST.funSigName) $ do
    sig' <- checkFunSig sig
    buildFunctionEnv sig'
    AST.FunDecl sig' <$> mapM checkStmt body

  checkVarDecl (AST.VarDecl sig inite) = enterDecl (sig ^. AST.varSigName) $ do
    let TypeScheme declGen declTy _ = varSigToTypeScheme sig
    (init', initTy) <- inferExpr inite
    requireType (TypeScheme declGen declTy AST.Get) initTy
    return $ AST.VarDecl sig init'

  -- checks that an expression is correct and infers its type
  inferExpr e@(AST.EVar v) = use (typeMap . at v) >>= \case
    Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Undeclared variable found in type checker. This is a bug. Did the program pass the scope checker?"
    Just scheme
      -- wrap variables of function type in an @ sign to prevent kOS from automatically calling functions
      | isFunctionScheme scheme -> return (AST.EAt e, scheme)
      | otherwise -> return (e, scheme)
  inferExpr (AST.EAccessor e _ field) = do
    (e', tysc@(TypeScheme _ _ access)) <- inferExpr e
    ty <- requireNonGeneric tysc
    requireAccess AST.Get access
    fty <- findField field ty
    -- if an accessor has a function type, again add the @ sign
    if isFunctionScheme fty
      then return $ (AST.EAt (AST.EAccessor e' (Just ty) field), fty)
      else return $ (AST.EAccessor e' (Just ty) field, fty)
  inferExpr (AST.EIndex e _ eidx) =  do
    (e', indexedTySc@(TypeScheme _ _ access)) <- inferExpr e
    indexedTy <- requireNonGeneric indexedTySc
    requireAccess AST.Get access
    (eidx', indexTy) <- inferExpr eidx
    TypeScheme [] idxField idxAccess <- findField "[]" indexedTy
    case idxField of
      AST.TypeFunction idxRet [reqIdxTy] _ -> do
        requireType (TypeScheme [] reqIdxTy AST.Get) indexTy
        return (AST.EIndex e' (Just indexedTy) eidx', TypeScheme [] idxRet idxAccess)
      _ -> return (AST.EIndex e' (Just unknownType) eidx', TypeScheme [] unknownType idxAccess)
  inferExpr (AST.EOp e1 eop e2) = do
    (e1', ts1@(TypeScheme _ ty1 acc1)) <- inferExpr e1
    (e2', ts2@(TypeScheme _ ty2 acc2)) <- inferExpr e2
    requireAccess AST.Get acc1
    requireAccess AST.Get acc2
    void $ requireNonGeneric ts1
    void $ requireNonGeneric ts2
    findOp eop ty1 ty2 >>= \case
      Nothing -> do
        messageWithContext MessageError $ MessageUnspecified $ PP.text "No matching operator found."
        return (AST.EOp e1' eop e2', TypeScheme [] unknownType AST.Get)
      Just retTy -> return (AST.EOp e1' eop e2', TypeScheme [] retTy AST.Get)
  inferExpr (AST.EUnOp op e) = do
    (e', ts@(TypeScheme _ ty acc)) <- inferExpr e
    requireAccess AST.Get acc
    requireNonGeneric ts
    case findUnOp op ty of
      Nothing -> do
        messageWithContext MessageError $ MessageUnspecified $ PP.text "No matching operator found."
        return (AST.EUnOp op e', TypeScheme [] unknownType AST.Get)
      Just retTy -> return (AST.EUnOp op e', TypeScheme [] retTy AST.Get)
  inferExpr (AST.ECall f tyargs args) = do
    -- infer type of the called function
    (f', TypeScheme generics ty access) <- inferExpr f
    requireAccess AST.Get access
    -- number of generics must match
    when (length tyargs /= length generics) $ messageWithContext MessageError $ MessageGenericTypeMismatch
    let gensubst = Map.fromList $ zip generics tyargs
    case ty of
      AST.TypeFunction retTy manTys optTys -> do
        -- must have at least all mandatory arguments, and not more than the total number of arguments
        when (length args < length manTys) $
          messageWithContext MessageError $ MessageNotEnoughArguments
        when (length args > length manTys + length optTys) $
          messageWithContext MessageError $ MessageTooManyArguments
        -- match arguments individually
        (args', argTys) <- unzip <$> traverse inferExpr args
        zipWithM_ requireType (map (\t -> TypeScheme [] (substituteGenerics gensubst t) AST.Get) $ manTys ++ optTys) argTys
        -- when calling something that was previously wrapped in an @ sign, remove it again
        let fNoAt = case f' of
              AST.EAt e -> e
              _         -> f'
        return (AST.ECall fNoAt tyargs args', TypeScheme [] (substituteGenerics gensubst retTy) AST.Get)
      other -> do
        messageWithContext MessageError $ MessageFunctionExpected other
        return (AST.ECall f' tyargs args, TypeScheme [] (AST.TypeGeneric (AST.ScopedLocal "__UNKNOWN__") []) AST.Get)
  inferExpr e@(AST.EScalar _) = return (e, TypeScheme [] scalarType AST.Get)
  inferExpr e@(AST.EString _) = return (e, TypeScheme [] stringType AST.Get)
  inferExpr e@(AST.EUnknown) = return (e, TypeScheme [] unknownType AST.GetOrSet)
  inferExpr (AST.ERecordInit recName tyArgs fields) = do
    let recTy = AST.TypeGeneric recName tyArgs
    fields' <- forM fields $ \(name, expr) -> do
      (TypeScheme fgen fty' _) <- findField name recTy
      (expr', ety) <- inferExpr expr
      requireType (TypeScheme fgen fty' AST.Get) ety
      return (name, expr')
    return (AST.ERecordInit recName tyArgs fields', TypeScheme [] recTy AST.Get)
  inferExpr (AST.ECast ty e) = do
    (e', ts@(TypeScheme _ _ acc)) <- inferExpr e
    _ <- requireNonGeneric ts
    return (AST.ECast ty e', TypeScheme [] ty acc)
  inferExpr (AST.ELambda params ret body) = enterScope $ enterDecl "<<lambda>>" $ do
    let lambdasig = AST.FunSig AST.Private ret "<<lambda>>" [] params Nothing
    buildFunctionEnv lambdasig
    body' <- mapM checkStmt body
    return (AST.ELambda params ret body', funSigToTypeScheme lambdasig)
  inferExpr (AST.EAt e) = do
    (e', ety) <- inferExpr e
    return (AST.EAt e', ety)

  -- checks that statements are correct
  checkStmt (AST.SDeclVar ty name init) = do
    let declty = TypeScheme [] ty AST.GetOrSet
    typeMap . at (AST.ScopedLocal name) .= Just declty
    (init', tyInit) <- inferExpr init
    requireType (TypeScheme [] ty AST.Get) tyInit
    return $ AST.SDeclVar ty name init'
  checkStmt (AST.SAssign lhs rhs) = do
    (lhs', TypeScheme gen lhsty lhsaccess) <- inferExpr lhs
    (rhs', rhsty) <- inferExpr rhs
    -- require Set access on lhs, but get access on rhs
    requireAccess AST.Set lhsaccess
    requireType (TypeScheme gen lhsty AST.Get) rhsty
    return $ AST.SAssign lhs' rhs'
  checkStmt s@(AST.SReturn mret) = use requiredReturnType >>= \case
    Nothing -> do
      messageWithContext MessageError $ MessageUnspecified $ PP.text "Cannot return from here"
      return s
    Just retType -> case mret of
      Nothing -> do
        requireType retType (TypeScheme [] voidType AST.Get)
        return s
      Just ret -> do
        (ret', actualType) <- inferExpr ret
        requireType retType actualType
        return $ AST.SReturn (Just ret')
  checkStmt (AST.SExpr ex) = AST.SExpr . view _1 <$> inferExpr ex -- just make sure some type IS can be inferred
  checkStmt (AST.SBlock stmts) = enterScope $ AST.SBlock <$> mapM checkStmt stmts
  checkStmt (AST.SIf cond sthen selse) = do
    (cond', condTy) <- inferExpr cond
    requireType (TypeScheme [] boolType AST.Get) condTy
    AST.SIf cond' <$> enterScope (traverse checkStmt sthen) <*> enterScope (traverse checkStmt selse)
  checkStmt (AST.SUntil cond body) = do
    (cond', condTy) <- inferExpr cond
    requireType (TypeScheme [] boolType AST.Get) condTy
    AST.SUntil cond' <$> enterScope (traverse checkStmt body)
  checkStmt (AST.SForEach ty name e body) = enterScope $ do
    let declty = TypeScheme [] ty AST.Get
    typeMap . at (AST.ScopedLocal name) .= Just declty
    (e', ety) <- inferExpr e
    requireType (TypeScheme [] (enumerableType ty) AST.Get) ety
    AST.SForEach ty name e' <$> enterScope (traverse checkStmt body)
  checkStmt (AST.SBreak) = pure AST.SBreak
  checkStmt (AST.SWait dur) = do
    (dur', durTy) <- inferExpr dur
    requireType (TypeScheme [] scalarType AST.Get) durTy
    return (AST.SWait dur')
  checkStmt (AST.SWaitUntil cond) = do
    (cond', condTy) <- inferExpr cond
    requireType (TypeScheme [] boolType AST.Get) condTy
    return (AST.SWaitUntil cond')
  checkStmt (AST.SLock var rhs) = do
    (_, TypeScheme gen lhsty lhsaccess) <- inferExpr (AST.EVar var)
    (rhs', rhsty) <- inferExpr rhs
    -- require Set access on lhs, but get access on rhs
    requireAccess AST.Set lhsaccess
    requireType (TypeScheme gen lhsty AST.Get) rhsty
    return $ AST.SLock var rhs'
  checkStmt s@(AST.SUnlock _) = pure s
  checkStmt (AST.SOn cond body) = do
    (cond', TypeScheme _ _ acc) <- inferExpr cond
    requireAccess AST.Get acc
    AST.SOn cond' <$> enterScope (requiredReturnType .= Just (TypeScheme [] boolType AST.Get) >> traverse checkStmt body)
  checkStmt (AST.SWhen cond body) = do
    (cond', condTy) <- inferExpr cond
    requireType (TypeScheme [] boolType AST.Get) condTy
    AST.SWhen cond' <$> enterScope (requiredReturnType .= Just (TypeScheme [] boolType AST.Get) >> traverse checkStmt body)

  checkFunSig (AST.FunSig vis ret name gens params outName) = do
    params' <- forM params $ \p@(AST.Param ty name expr) -> case expr of
      Nothing -> return p
      Just e -> do
        (e', ety) <- inferExpr e
        requireType (TypeScheme [] ty AST.Get) ety
        return (AST.Param ty name (Just e'))
    return (AST.FunSig vis ret name gens params' outName)

  -- predefined types
  -- FIXME: make it so that these do not require a hard-coded type name
  structType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Builtin", "Structure"]) []
  scalarType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Builtin", "Scalar"]) []
  stringType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Builtin", "String"]) []
  boolType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Builtin", "Boolean"]) []
  voidType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Builtin", "Void"]) []
  vectorType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Math", "Vector"]) []
  directionType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Math", "Direction"]) []
  timeSpanType = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Time", "TimeSpan"]) []
  enumerableType a = AST.TypeGeneric (AST.ScopedGlobal ["KOS", "Collections", "Enumerable"]) [a]

  -- table containing possible overloads of binary operators
  operatorOverloads binOp = case binOp of
    AST.BinOpPlus -> [(scalarType, scalarType, scalarType)
                     ,(vectorType, vectorType, vectorType)
                     ,(stringType, stringType, stringType)
                     ,(timeSpanType, scalarType, timeSpanType)
                     ,(scalarType, timeSpanType, timeSpanType)]
    AST.BinOpMinus -> [(scalarType, scalarType, scalarType)
                     ,(vectorType, vectorType, vectorType)
                     ,(timeSpanType, scalarType, timeSpanType)
                     ,(scalarType, timeSpanType, timeSpanType)]
    AST.BinOpMult -> [(scalarType, scalarType, scalarType)
                     ,(vectorType, vectorType, scalarType)
                     ,(vectorType, scalarType, vectorType)
                     ,(scalarType, vectorType, vectorType)]
    AST.BinOpDiv -> [(scalarType, scalarType, scalarType)
                     ,(vectorType, scalarType, scalarType)
                     ,(scalarType, vectorType, vectorType)]
    AST.BinOpPow -> [(scalarType, scalarType, scalarType)]
    AST.BinOpAnd -> [(boolType, boolType, boolType)]
    AST.BinOpOr -> [(boolType, boolType, boolType)]
    AST.BinOpEq -> [(structType, structType, boolType)]
    AST.BinOpLeq -> [(structType, structType, boolType)]
    AST.BinOpGeq -> [(structType, structType, boolType)]
    AST.BinOpNeq -> [(structType, structType, boolType)]
    AST.BinOpGreater -> [(structType, structType, boolType)]
    AST.BinOpLess -> [(structType, structType, boolType)]

  -- searches a matching overload of an operator
  findOp binOp ty1 ty2 = fmap (view _2) . find (view _1) <$> mapM check (operatorOverloads binOp) where
    check (arg1, arg2, ret) = (\a b -> (a && b, ret)) <$> isSubtypeOf Covariant ty1 arg1 <*> isSubtypeOf Covariant ty2 arg2

  unaryOperatorOverloads unOp = case unOp of
    AST.UnOpNegate -> [(scalarType, scalarType), (vectorType, vectorType), (directionType, directionType)]
    AST.UnOpNot -> [(boolType, boolType)]

  findUnOp unOp ty = view _2 <$> find (\(a,_) -> a == ty) (unaryOperatorOverloads unOp)

  -- saves current type environment and restores it on exit
  enterScope action = do
    env <- get
    r <- action
    put env
    return r

-- * Type Predicates

-- | Is the type scheme representing a function?
isFunctionScheme :: TypeScheme -> Bool
isFunctionScheme (TypeScheme _ (AST.TypeFunction _ _ _) _) = True
isFunctionScheme _                                         = False

-- | Checks whether the type scheme is generic, and emits an error message in that case
requireNonGeneric :: MonadCompiler MessageContent m => TypeScheme -> m (AST.Type AST.ScopedName)
requireNonGeneric (TypeScheme [] ty _) = pure ty
requireNonGeneric (TypeScheme _ ty _) = do
  messageWithContext MessageError $ MessageGenericTypeMismatch
  return ty

-- | Substitutes generic variables in a type.
substituteGenerics :: Map AST.Ident (AST.Type AST.ScopedName) -> AST.Type AST.ScopedName -> AST.Type AST.ScopedName
substituteGenerics subst = go where
  -- cannot apply generics to a generic variable anyway
  go v@(AST.TypeGeneric (AST.ScopedLocal l) []) = Map.findWithDefault v l subst
  go (AST.TypeGeneric name args) = AST.TypeGeneric name (map go args)
  go (AST.TypeFunction ret args opts) = AST.TypeFunction (go ret) (map go args) (map go opts)

-- | Enforces that something is accessible in the required way.
requireAccess :: MonadCompiler MessageContent m => AST.Accessibility -> AST.Accessibility -> m ()
requireAccess required actual = when (not $ hasAccess required actual) $
  messageWithContext MessageError $ MessageWrongAccessibility required

-- | @hasAccess a b@ checks whether @b@ can be accessed as required by @a@.
hasAccess :: AST.Accessibility -> AST.Accessibility -> Bool
hasAccess required actual = case required of
  AST.Get      -> actual /= AST.Set
  AST.Set      -> actual /= AST.Get
  AST.GetOrSet -> actual == AST.GetOrSet

-- | Checks inheritance hierarchies for circular definitions.
validateInheritance :: MonadCompiler MessageContent m => AST.ScopedName -> StateT TypeEnv m ()
validateInheritance = go Set.empty where
  go visited name
    | Set.member name visited = messageWithContext MessageError $ MessageCircularInheritance name
    | otherwise = use (globalTypeMap . at name) >>= \case
        Nothing -> return ()
        Just (ScopedStruct ssig) -> case ssig ^. AST.structSigSuper of
          Nothing -> return ()
          Just (AST.TypeGeneric superTy _) -> go (Set.insert name visited) superTy
          Just (AST.TypeFunction _ _ _) -> return () -- does it make sense to allow deriving from functions?

-- * Subtyping

-- | Variance when checking subtyping
data Variance
  = Covariant      -- ^ checking covariantly (A is a subtype of B if A derives from B)
  | Contravariant  -- ^ checking contravariantly (A is a subtype of B if B derives from A)
  | Invariant      -- ^ checking invariantly (A is a subtype of B if A is equal to B)

-- | Flips the variance (needed when checking functions, as they are contravariant in their arguments).
flipVariance :: Variance -> Variance
flipVariance Covariant     = Contravariant
flipVariance Contravariant = Covariant
flipVariance Invariant     = Invariant

-- | @requireType required actual@ requires that @actual@ is a subtype of @required@.
requireType :: MonadCompiler MessageContent m => TypeScheme -> TypeScheme -> StateT TypeEnv m ()
requireType (TypeScheme gen1 ty1 acc1) (TypeScheme gen2 ty2 acc2)
  | length gen1 /= length gen2 = messageWithContext MessageError $ MessageGenericTypeMismatch
  | otherwise =  do
      isSub <- isSubtypeOf Covariant ty2 (substituteGenerics (Map.fromList $ zip gen1 (map (\g -> AST.TypeGeneric (AST.ScopedLocal g) []) gen2)) ty1)
      when (not $ isSub && hasAccess acc1 acc2) $
        messageWithContext MessageError $ MessageTypesNotEqual ty1 ty2

-- | @isSubSchemeOf sub super@ checks if @sub@ and @super@ have the same number
-- of generic arguments and that @sub@ can be instantiated to be a subtype of @super@.
isSubSchemeOf :: MonadCompiler MessageContent m => TypeScheme -> TypeScheme -> StateT TypeEnv m Bool
isSubSchemeOf (TypeScheme gen1 ty1 acc1) (TypeScheme gen2 ty2 acc2)
  | length gen1 /= length gen2 = return False
  | otherwise = do
      isSub <- isSubtypeOf Covariant (substituteGenerics (Map.fromList $ zip gen1 (map (\g -> AST.TypeGeneric (AST.ScopedLocal g) []) gen2)) ty1) ty2
      return $ isSub && hasAccess acc1 acc2

-- | @isSubtypeOf var sub super@ checks whether @sub@ is a subtype of @super@ using the current variance information.
-- For example, when checking covariantly, @Scalar@ is a subtype of @Structure@, and a function @Scalar(Structure)@
-- is a subtype of @Structure(Scalar)@. Note that the variance has been flipped when checking the arguments of the functions.
-- This is because any function that can take a @Structure@ can also take a @Scalar@.
-- Generic type arguments are checked invariantly currently, because they could occur in both covariant and contravariant positions.
isSubtypeOf :: MonadCompiler MessageContent m => Variance -> AST.Type AST.ScopedName -> AST.Type AST.ScopedName -> StateT TypeEnv m Bool
isSubtypeOf = compareTypes where
    compareTypes var (AST.TypeGeneric n1 args1) (AST.TypeGeneric n2 args2) = do
      allowedCast <- checkCast var n1 args1 n2 args2
      return $ allowedCast && length args1 == length args2
    compareTypes var (AST.TypeFunction ret1 args1 opts1) (AST.TypeFunction ret2 args2 opts2) = do
      c1 <- compareTypes var ret1 ret2
      if length args1 /= length args2 || length opts1 /= length opts2
        then return False
        else do
           c2 <- and <$> zipWithM (compareTypes $ flipVariance var) args1 args2
           c3 <- and <$> zipWithM (compareTypes $ flipVariance var) opts1 opts2
           return $ c1 && c2 && c3
    compareTypes _ _ _ = return False

    checkCast Invariant n1 args1 n2 args2 = return $ n1 == n2 && args1 == args2
    checkCast Covariant n1 args1 n2 args2 = (n1, args1) `isSubStructOf` (n2, args2)
    checkCast Contravariant n1 args1 n2 args2 = (n2, args2) `isSubStructOf` (n1, args1)

-- | Checks whether the first structure derives from the second.
isSubStructOf :: MonadCompiler MessageContent m
              => (AST.ScopedName, [AST.Type AST.ScopedName]) -- ^ name and generic type arguments of the first struct
              -> (AST.ScopedName, [AST.Type AST.ScopedName]) -- ^ name and generic type arguments of the second struct
              -> StateT TypeEnv m Bool
isSubStructOf = go Set.empty where
  go visited (n1, tyargs1) (n2, tyargs2)
    | n1 == n2 && tyargs1 == tyargs2 = return True
    | Set.member n1 visited = return False -- circular inheritance
    | otherwise = do
        ty1 <- use (globalTypeMap . at n1)
        case ty1 of
          Just (ScopedStruct ssig)
            | length (ssig ^. AST.structSigGenerics) /= length tyargs1 -> return False
            | otherwise -> do
                -- substitute generic variables of *the structure*
                let gensubst = Map.fromList $ zip (ssig ^. AST.structSigGenerics) tyargs1
                case ssig ^. AST.structSigSuper of -- try super structure
                  Just (AST.TypeGeneric nsuper argsSuper) -> go (Set.insert n1 visited) (nsuper, map (substituteGenerics gensubst) argsSuper) (n2, tyargs2)
                  _ -> return False
          Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Unknown type encountered during type checking:" PP.<+> PP.pretty n1 PP.<+> PP.text "This is a bug."

-- * Managing Type Environment

-- | Builds the environment required for checking a function body by bringing all the parameters in scope.
buildFunctionEnv :: Monad m => AST.FunSig AST.ScopedName -> StateT TypeEnv m ()
buildFunctionEnv sig = do
  requiredReturnType .= Just (TypeScheme [] (sig ^. AST.funSigReturnType) AST.Get)
  forMOf_ (AST.funSigParameters . folded) sig $ \(AST.Param ty name _) ->
    typeMap . at (AST.ScopedLocal name) .= Just (TypeScheme [] ty AST.GetOrSet)

-- | Generate a type scheme from a function.
funSigToTypeScheme :: AST.FunSig AST.ScopedName -> TypeScheme
funSigToTypeScheme fsig =
  let ret = fsig ^. AST.funSigReturnType
      args = toListOf (AST.funSigParameters . folded . filtered AST.isMandatory . AST.paramType) fsig
      opts = toListOf (AST.funSigParameters . folded . filtered AST.isOptional . AST.paramType) fsig
  in TypeScheme (fsig ^. AST.funSigGenerics) (AST.TypeFunction ret args opts) AST.Get

-- | Generate a type scheme from a variable.
varSigToTypeScheme :: AST.VarSig AST.ScopedName -> TypeScheme
varSigToTypeScheme vsig = TypeScheme [] (vsig ^. AST.varSigType) (vsig ^. AST.varSigAccess)

-- | From an indexing operation in a structure.
indexSigToTypeScheme :: AST.IndexSig AST.ScopedName -> TypeScheme
indexSigToTypeScheme isig = TypeScheme [] (AST.TypeFunction (isig ^. AST.indexSigReturnType) [isig ^. AST.indexSigIndexType] []) (isig ^. AST.indexSigAccess)

-- | Adds all global definitions to the scope.
populateGlobalTypeEnv :: Monad m => Map AST.ModuleName ScopedModule -> StateT TypeEnv m ()
populateGlobalTypeEnv = imapMOf_ (folded . scopedModuleVars . ifolded) $ \name sig ->
  case sig of
    ScopedFun fsig -> typeMap . at name .= Just (funSigToTypeScheme fsig)
    ScopedVar vsig -> typeMap . at name .= Just (varSigToTypeScheme vsig)

-- * Querying Type Environment

-- | Given the name of a field and a name of a type, it searches for the field
-- in the types inheritance hierarchy. If successful, the type of the field is
-- returned, otherwise, an error is generated.
findField :: (MonadState TypeEnv m, MonadCompiler MessageContent m)
          => AST.Ident -> AST.Type AST.ScopedName -> m TypeScheme
findField fieldName startedTy = go Set.empty startedTy where
  notFound = do
        messageWithContext MessageError $ MessageFieldNotFound startedTy fieldName
        return (TypeScheme [] unknownType AST.GetOrSet)
  go visited ty
    | Set.member ty visited = notFound
    | otherwise = case ty of
      AST.TypeGeneric n tyargs -> use (globalTypeMap . at n) >>= \case
        Nothing -> notFound
        Just scty -> case scty of
          ScopedStruct ssig -> do
            when (length (ssig ^. AST.structSigGenerics) /= length tyargs) $ messageWithContext MessageError $ MessageGenericTypeMismatch
            -- substitute generic variables of *the structure*
            let gensubst = Map.fromList $ zip (ssig ^. AST.structSigGenerics) tyargs
            case findFieldSig fieldName (ssig ^. AST.structSigFields) of
              Just (TypeScheme genvars ty acc) -> do
                -- exempt shadowed generic variables of the field from the substitution
                let gensubst' = foldr Map.delete gensubst genvars
                return $ TypeScheme genvars (substituteGenerics gensubst' ty) acc
              Nothing -> case ssig ^. AST.structSigSuper of -- try super structure
                Nothing -> notFound
                Just tySuper -> go (Set.insert ty visited) (substituteGenerics gensubst tySuper)
      -- TODO: implement special logic for the ":CALL" and ":BIND" fields that are present on function types
      AST.TypeFunction _ _ _ -> notFound

-- | Searches for a field in a list of fields
findFieldSig :: AST.Ident -> [AST.FieldSig AST.ScopedName] -> Maybe TypeScheme
findFieldSig _ [] = Nothing
findFieldSig fieldName (sig:sigs) = case sig of
  AST.FieldFunSig fsig
    | fsig ^. AST.funSigName == fieldName -> Just (funSigToTypeScheme fsig)
  AST.FieldVarSig vsig
    | vsig ^. AST.varSigName == fieldName -> Just (varSigToTypeScheme vsig)
  AST.FieldIndexSig isig
    | fieldName == "[]" -> Just (indexSigToTypeScheme isig)
  _ -> findFieldSig fieldName sigs

-- | Special type that will be used when the type checker is unable to infer the
-- type, but it is desirable to continue the type checking process in order to first
-- find as many errors as possible before showing them to the user.
unknownType :: AST.Type AST.ScopedName
unknownType = AST.TypeGeneric (AST.ScopedLocal "<unknown>") []
