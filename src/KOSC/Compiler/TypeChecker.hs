{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-| This module implements type checking functionality. -}
module KOSC.Compiler.TypeChecker where

import           Control.Lens
import Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Semigroup
import           Data.List
import           Data.Maybe
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           System.Directory
import           System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Trifecta

import qualified KOSC.Language.AST                     as AST
import qualified KOSC.Language.Parser                  as Parser
import KOSC.Compiler.Common
import KOSC.Compiler.ImportResolution
import KOSC.Compiler.ScopeChecker

data TypeScheme = TypeScheme [AST.Ident] (AST.Type AST.ScopedName) AST.Accessibility

data TypeEnv = TypeEnv
  { _typeMap            :: Map AST.ScopedName TypeScheme
  , _requiredReturnType :: Maybe TypeScheme
  }

makeLenses ''TypeEnv

typeChecker :: Monad m => Map AST.ModuleName ScopedModule -> ScopedModule -> KOSCCompilerT m ()
typeChecker imports inputMod = evalStateT checkModule initialEnv where
  initialEnv = TypeEnv Map.empty Nothing

  globalTypeMap = foldOf (folded . scopedModuleTypes) imports

  checkModule = do
    populateGlobalTypeEnv imports
    enterModule (inputMod ^. scopedModuleAST . AST.moduleName) $
      mapMOf_ (scopedModuleAST . AST.declarations . folded) checkDecl inputMod

  checkDecl (AST.DeclImport _) = pure ()
  checkDecl (AST.DeclFun fdecl) = checkFunDecl fdecl
  checkDecl (AST.DeclBuiltin _) = pure ()

  checkFunDecl (AST.FunDecl sig body) = enterScope $ enterDecl (sig ^. AST.funSigName) $ do
    buildFunctionEnv sig
    mapM_ checkStmt body

  -- checks that an expression is correct and infers its type
  inferExpr (AST.EVar v) = use (typeMap . at v) >>= \case
    Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Undeclared variable found in type checker. This is a bug. Did the program pass the scope checker?"
    Just scheme -> return scheme
  inferExpr (AST.EAccessor e field) = do
    tysc@(TypeScheme _ _ access) <- inferExpr e
    ty <- requireNonGeneric tysc
    requireAccess AST.Get access
    findField field ty ty
  inferExpr (AST.EIndex e eidx) =  do
    indexedTySc@(TypeScheme _ _ access) <- inferExpr e
    indexedTy <- requireNonGeneric indexedTySc
    requireAccess AST.Get access
    indexTy <- inferExpr eidx
    TypeScheme [] idxField idxAccess <- findField "[]" indexedTy indexedTy
    case idxField of
      AST.TypeFunction idxRet [reqIdxTy] _ -> do
        requireType (TypeScheme [] reqIdxTy AST.Get) indexTy
        return $ TypeScheme [] idxRet idxAccess
      _ -> return $ TypeScheme [] unknownType idxAccess
  inferExpr (AST.EOp e1 op e2)
    | AST.isArithmeticOp op = do
        ty1 <- inferExpr e1
        ty2 <- inferExpr e2
        let getscalar = TypeScheme [] scalarType AST.Get
        requireType getscalar ty1
        requireType getscalar ty2
        return getscalar
    | otherwise = return $ TypeScheme [] unknownType AST.GetOrSet
  inferExpr (AST.ECall f tyargs args) = do
    -- infer type of the called function
    TypeScheme generics ty access <- inferExpr f
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
        argTys <- traverse inferExpr args
        zipWithM_ requireType (map (\t -> TypeScheme [] (substituteGenerics gensubst t) AST.Get) $ manTys ++ optTys) argTys
        return $ TypeScheme [] (substituteGenerics gensubst retTy) AST.Get
      other -> do
        messageWithContext MessageError $ MessageFunctionExpected other
        return $ TypeScheme [] (AST.TypeGeneric (AST.ScopedLocal "__UNKNOWN__") []) AST.Get
  inferExpr (AST.EScalar d) = pure $ TypeScheme [] scalarType AST.Get
  inferExpr (AST.EString s) = pure $ TypeScheme [] stringType AST.Get

  -- checks that statements are correct
  checkStmt (AST.SDeclVar ty name init) = do
    let declty = TypeScheme [] ty AST.GetOrSet
    typeMap . at (AST.ScopedLocal name) .= Just declty
    tyInit <- inferExpr init
    requireType (TypeScheme [] ty AST.Get) tyInit
  checkStmt (AST.SAssign lhs rhs) = do
    (TypeScheme gen lhsty lhsaccess) <- inferExpr lhs
    rhsty <- inferExpr rhs
    -- require Set access on lhs, but get access on rhs
    requireAccess AST.Set lhsaccess
    requireType (TypeScheme gen lhsty AST.Get) rhsty
  checkStmt (AST.SReturn ret) = use requiredReturnType >>= \case
    Nothing -> messageWithContext MessageError $ MessageUnspecified $ PP.text "Cannot return from here"
    Just retType -> do
      actualType <- inferExpr ret
      requireType retType actualType
  checkStmt (AST.SExpr ex) = void $ inferExpr ex -- just make sure some type IS can be inferred
  checkStmt (AST.SBlock stmts) = enterScope $ mapM_ checkStmt stmts

  -- searches a field of a type
  findField fieldName startedTy ty =
    let notFound = do
          messageWithContext MessageError $ MessageFieldNotFound startedTy fieldName
          return (TypeScheme [] unknownType AST.GetOrSet)
    in case ty of
         AST.TypeGeneric n tyargs -> case Map.lookup n globalTypeMap of
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
                   Just tySuper -> findField fieldName startedTy (substituteGenerics gensubst tySuper)
  -- TODO: implement special logic for the ":CALL" and ":BIND" fields that are present on function types
         AST.TypeFunction _ _ _ -> notFound

  -- searches for a field in a list of fields
  findFieldSig fieldName [] = Nothing
  findFieldSig fieldName (sig:sigs) = case sig of
    AST.FieldFunSig fsig
      | fsig ^. AST.funSigName == fieldName -> Just (funSigToTypeScheme fsig)
    AST.FieldVarSig vsig
      | vsig ^. AST.varSigName == fieldName -> Just (varSigToTypeScheme vsig)
    AST.FieldIndexSig isig
      | fieldName == "[]" -> Just (indexSigToTypeScheme isig)
    _ -> findFieldSig fieldName sigs

  -- predefined types
  -- FIXME: make it so that these do not require a hard-coded type name
  scalarType = AST.TypeGeneric (AST.ScopedGlobal ["Builtin", "Scalar"]) []
  stringType = AST.TypeGeneric (AST.ScopedGlobal ["Builtin", "String"]) []
  unknownType = AST.TypeGeneric (AST.ScopedLocal "__UNKNOWN__") []

  -- saves current type environment and restores it on exit
  enterScope action = do
    env <- get
    r <- action
    put env
    return r

requireNonGeneric :: MonadCompiler MessageContent m => TypeScheme -> m (AST.Type AST.ScopedName)
requireNonGeneric (TypeScheme [] ty _) = pure ty
requireNonGeneric (TypeScheme gen ty _) = do
  messageWithContext MessageError $ MessageGenericTypeMismatch
  return ty

substituteGenerics :: Map AST.Ident (AST.Type AST.ScopedName) -> AST.Type AST.ScopedName -> AST.Type AST.ScopedName
substituteGenerics subst = go where
  -- cannot apply generics to a generic variable anyway
  go v@(AST.TypeGeneric (AST.ScopedLocal l) []) = Map.findWithDefault v l subst
  go (AST.TypeGeneric name args) = AST.TypeGeneric name (map go args)
  go (AST.TypeFunction ret args opts) = AST.TypeFunction (go ret) (map go args) (map go opts)

requireAccess :: MonadCompiler MessageContent m => AST.Accessibility -> AST.Accessibility -> m ()
requireAccess required actual = case required of
  AST.Get -> when (actual == AST.Set) msg
  AST.Set -> when (actual == AST.Get) msg
  AST.GetOrSet -> when (actual /= AST.GetOrSet) msg
  where
    msg = messageWithContext MessageError $ MessageWrongAccessibility required

-- TODO: perform subtyping check instead of equality
requireType :: MonadCompiler MessageContent m => TypeScheme -> TypeScheme -> m ()
requireType (TypeScheme gen1 ty1 acc1) (TypeScheme gen2 ty2 acc2)
  | length gen1 /= length gen2 = messageWithContext MessageError $ MessageGenericTypeMismatch
  | otherwise = do
      requireAccess acc1 acc2
      compareTypes (substituteGenerics (Map.fromList $ zip gen1 (map (\g -> AST.TypeGeneric (AST.ScopedLocal g) []) gen2)) ty1) ty2
  where
    compareTypes t1@(AST.TypeGeneric n1 args1) t2@(AST.TypeGeneric n2 args2) = do
      when (n1 /= n2 || length args1 /= length args2) $
        messageWithContext MessageError $ MessageTypesNotEqual t1 t2
      zipWithM_ compareTypes args1 args2
    compareTypes t1@(AST.TypeFunction ret1 args1 opts1) t2@(AST.TypeFunction ret2 args2 opts2) = do
      compareTypes ret1 ret2
      when (length args1 /= length args2 || length opts1 /= length opts2) $
        messageWithContext MessageError $ MessageTypesNotEqual t1 t2
      zipWithM_ compareTypes args1 args2
      zipWithM_ compareTypes opts1 opts2
    compareTypes t1 t2 = messageWithContext MessageError $ MessageTypesNotEqual t1 t2

buildFunctionEnv :: Monad m => AST.FunSig AST.ScopedName -> StateT TypeEnv m ()
buildFunctionEnv sig = do
  requiredReturnType .= Just (TypeScheme [] (sig ^. AST.funSigReturnType) AST.Get)
  forMOf_ (AST.funSigParameters . folded) sig $ \(AST.Param ty name _) ->
    typeMap . at (AST.ScopedLocal name) .= Just (TypeScheme [] ty AST.GetOrSet)

funSigToTypeScheme :: AST.FunSig AST.ScopedName -> TypeScheme
funSigToTypeScheme fsig =
  let ret = fsig ^. AST.funSigReturnType
      args = toListOf (AST.funSigParameters . folded . filtered AST.isMandatory . AST.paramType) fsig
      opts = toListOf (AST.funSigParameters . folded . filtered AST.isOptional . AST.paramType) fsig
  in TypeScheme (fsig ^. AST.funSigGenerics) (AST.TypeFunction ret args opts) AST.Get

varSigToTypeScheme :: AST.VarSig AST.ScopedName -> TypeScheme
varSigToTypeScheme vsig = TypeScheme [] (vsig ^. AST.varSigType) (vsig ^. AST.varSigAccess)

indexSigToTypeScheme :: AST.IndexSig AST.ScopedName -> TypeScheme
indexSigToTypeScheme isig = TypeScheme [] (AST.TypeFunction (isig ^. AST.indexSigReturnType) [isig ^. AST.indexSigIndexType] []) (isig ^. AST.indexSigAccess)

populateGlobalTypeEnv :: Monad m => Map AST.ModuleName ScopedModule -> StateT TypeEnv m ()
populateGlobalTypeEnv = imapMOf_ (folded . scopedModuleVars . ifolded) $ \name sig ->
  case sig of
    ScopedFun fsig -> typeMap . at name .= Just (funSigToTypeScheme fsig)
    ScopedVar vsig -> typeMap . at name .= Just (varSigToTypeScheme vsig)
