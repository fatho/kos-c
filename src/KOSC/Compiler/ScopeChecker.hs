{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-| This module implements scope checking functionality. Raw names in the AST are
  replaced by fully qualified names that also include the module they are coming
  from. -}
module KOSC.Compiler.ScopeChecker where


import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Foldable
import           Data.Semigroup
import           Data.List
import           Data.Maybe
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified KOSC.Language.AST                     as AST
import KOSC.Compiler.Common
import KOSC.Compiler.ImportResolution

-- | A scope consists of mappings from raw names to the respective variables or types.
data Scope = Scope
  { _termScope :: Map AST.RawName AST.ScopedName -- ^ unqualified term identifiers in scope
  , _typeScope :: Map AST.RawName AST.ScopedName -- ^ unqualified type identifiers in scope
  }

-- | Scoped, exported type.
data ScopedTypeExport = ScopedStruct (AST.StructSig AST.ScopedName)
  deriving (Read, Show)

-- | Scoped, exported variable or function.
data ScopedTermExport = ScopedFun (AST.FunSig AST.ScopedName) | ScopedVar (AST.VarSig AST.ScopedName)
  deriving (Read, Show)

-- | A scoped module with extra information populated by the scope checker.
data ScopedModule = ScopedModule
  { _scopedModuleAST   :: AST.Module AST.ScopedName
  , _scopedModuleTypes :: Map AST.ScopedName ScopedTypeExport
  , _scopedModuleVars  :: Map AST.ScopedName ScopedTermExport
  } deriving (Read, Show)

makeLenses ''ScopedModule

instance Monoid Scope where
  mempty = Scope Map.empty Map.empty
  mappend (Scope te1 ty1) (Scope te2 ty2) = Scope (te1 <> te2) (ty1 <> ty2)

makeLenses ''Scope

-- | Resolves all references in a module to module-qualified names
scopeChecker :: Monad m => ImportResolution -> AST.RawModule -> KOSCCompilerT m ScopedModule
scopeChecker imports inputMod = initialScope >>= evalStateT checkedMod where
  inputModuleName = view AST.moduleName inputMod

  initialScope = enterModule inputModuleName $ do
    -- list of all imported modules, and the current module itself
    let imported = (AST.ImportDecl (view AST.moduleName inputMod) Nothing True)
                   : [ decl | AST.DeclImport decl <- view AST.declarations inputMod ]
    -- build a scope for all imports
    fold <$> traverse importedScope imported

  importedScope importDecl = do
    let importedName :: AST.ModuleName
        importedName = view AST.importModuleName importDecl
        modInfo' = imports ^. importResolutionModules . at importedName
    case modInfo' of
      Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Tried to generate scope of non-imported module" PP.<+> PP.pretty importedName PP.<> PP.text ". This is likely a bug and should be fixed."
      Just modInfo -> do
        -- get exported term and type names of the given module
        let defTerms = Set.toList $ modInfo ^. moduleInfoExportedVars
            defTypes = Set.toList $ modInfo ^. moduleInfoExportedTypes
            -- create scope maps
            qualName = fromMaybe importedName (view AST.importAlias importDecl)
            -- use alias name, if given
            qualTermMap = Map.fromList [ (AST.makeRawName qualName n, AST.makeGlobalName importedName n) | n <- defTerms ]
            qualTypeMap = Map.fromList [ (AST.makeRawName qualName n, AST.makeGlobalName importedName n) | n <- defTypes ]
            -- also import unqualified, if requested
            unqualified = view AST.importUnqualified importDecl
            uqualTermMap = if unqualified then Map.fromList [ (AST.RawName [n], AST.makeGlobalName importedName n) | n <- defTerms ] else Map.empty
            uqualTypeMap = if unqualified then Map.fromList [ (AST.RawName [n], AST.makeGlobalName importedName n) | n <- defTypes ] else Map.empty
        return $ Scope (Map.unionWith (<>) qualTermMap uqualTermMap) (Map.unionWith (<>) qualTypeMap uqualTypeMap)

  checkedMod = enterModule (view AST.moduleName inputMod) $ do
    scopedAST <- mapMOf (AST.declarations . traversed) checkDecl inputMod
    let typeExports = Map.fromList $ over (traversed . _1) (AST.makeGlobalName inputModuleName)
          $ concatMap scopedTypeExport $ view AST.declarations scopedAST
        termExports = Map.fromList $ over (traversed . _1) (AST.makeGlobalName inputModuleName)
          $ concatMap scopedTermExport $ view AST.declarations scopedAST
    return $ ScopedModule scopedAST typeExports termExports

  scopedTypeExport (AST.DeclImport _) = []
  scopedTypeExport (AST.DeclFun _) = []
  scopedTypeExport (AST.DeclVar _) = []
  scopedTypeExport (AST.DeclRec r) = [ (r ^. AST.recDeclName, ScopedStruct $ makeRecordSig r) ]
  scopedTypeExport (AST.DeclBuiltin bi) = case bi of
    AST.BuiltinStruct sig -> [(sig ^. AST.structSigName, ScopedStruct sig)]
    _ -> []

  scopedTermExport (AST.DeclImport _) = []
  scopedTermExport (AST.DeclFun (AST.FunDecl fsig _)) = [(fsig ^. AST.funSigName, ScopedFun fsig)]
  scopedTermExport (AST.DeclVar (AST.VarDecl vsig _)) = [(vsig ^. AST.varSigName, ScopedVar vsig)]
  scopedTermExport (AST.DeclRec _) = []
  scopedTermExport (AST.DeclBuiltin bi) = case bi of
    AST.BuiltinFun fsig -> [(fsig ^. AST.funSigName,  ScopedFun fsig)]
    AST.BuiltinVar vsig -> [(vsig ^. AST.varSigName,  ScopedVar vsig)]
    _ -> []

  checkDecl (AST.DeclImport i) = pure (AST.DeclImport i)
  checkDecl (AST.DeclFun fd) = AST.DeclFun <$> checkFun fd
  checkDecl (AST.DeclVar vd) = AST.DeclVar <$> checkVar vd
  checkDecl (AST.DeclRec r) = AST.DeclRec <$> checkRec r
  checkDecl (AST.DeclBuiltin bi) = AST.DeclBuiltin <$> checkBuiltin bi

  checkFun (AST.FunDecl sig stmts) = localScope $ do
    sig' <- checkFunSig sig
    stmts' <- enterDecl (sig ^. AST.funSigName) $ mapM checkStmt stmts
    return (AST.FunDecl sig' stmts')

  checkVar (AST.VarDecl sig init) = do
    sig' <- checkVarSig sig
    init' <- enterDecl (sig ^. AST.varSigName) $ checkExpr init
    return (AST.VarDecl sig' init')

  checkRec (AST.RecDecl vis name gens vars) = localScope $ enterDecl name $ do
    insertGenerics gens
    AST.RecDecl vis name gens <$> traverse checkVarSig vars

  checkName mapLens n = do
    scope <- use mapLens
    case Map.lookup n scope of
      Nothing -> do
        lift $ messageWithContext MessageError (MessageNotInScope n)
        -- just return a random name so that we can continue scope checking and possibly find some more errors
        return (AST.ScopedGlobal (AST.rawNameParts n))
      Just a@(AST.ScopedAmbiguous names) -> do
        lift $ messageWithContext MessageError (MessageAmbiguousName n a)
        -- just return a random name so that we can continue scope checking and possibly find some more errors
        return (AST.ScopedGlobal $ head names)
      Just fine -> pure fine

  checkTypeName = checkName typeScope

  checkTermName = checkName termScope

  checkType (AST.TypeGeneric n args) = AST.TypeGeneric <$> checkTypeName n <*> traverse checkType args
  checkType (AST.TypeFunction ret args opts) = AST.TypeFunction <$> checkType ret <*> traverse checkType args <*> traverse checkType opts

  checkParam (AST.Param ty name expr) = AST.Param <$> checkType ty <*> pure name <*> traverse checkExpr expr

  checkExpr (AST.EVar v) = AST.EVar <$> checkTermName v
  checkExpr (AST.EAccessor e _ field) = AST.EAccessor <$> checkExpr e <*> pure Nothing <*> pure field
  checkExpr (AST.EIndex e _ eidx) = AST.EIndex <$> checkExpr e <*> pure Nothing <*> checkExpr eidx
  checkExpr (AST.EOp e1 op e2) = AST.EOp <$> checkExpr e1 <*> pure op <*> checkExpr e2
  checkExpr (AST.EUnOp op e) = AST.EUnOp op <$> checkExpr e
  checkExpr (AST.ECall f tyargs args) = AST.ECall <$> checkExpr f <*> traverse checkType tyargs <*> traverse checkExpr args
  checkExpr (AST.EScalar d) = pure $ AST.EScalar d
  checkExpr (AST.EString s) = pure $ AST.EString s
  checkExpr (AST.EUnknown) = pure $ AST.EUnknown
  checkExpr (AST.ERecordInit name tyArgs fields) = AST.ERecordInit <$> checkTypeName name <*> traverse checkType tyArgs <*> mapMOf (traversed . _2) checkExpr fields
  checkExpr (AST.ECast ty e) = AST.ECast <$> checkType ty <*> checkExpr e
  checkExpr (AST.ELambda params ret body) = do
    AST.FunSig _ ret' _ _ params' _ <- checkFunSig (AST.FunSig AST.Private ret "<<lambda>>" [] params Nothing)
    AST.ELambda params' ret' <$> enterDecl "<<lambda>>" (localScope (traverse checkStmt body))
  checkExpr (AST.EAt e) = AST.EAt <$> checkExpr e -- should never occur at this stage, but this makes the warnings disappear

  checkStmt (AST.SDeclVar ty name init) = do
    insertLocalVars [name]
    AST.SDeclVar <$> checkType ty <*> pure name <*> checkExpr init
  checkStmt (AST.SAssign lhs rhs) = AST.SAssign <$> checkExpr lhs <*> checkExpr rhs
  checkStmt (AST.SReturn ret) = AST.SReturn <$> checkExpr ret
  checkStmt (AST.SExpr ex) = AST.SExpr <$> checkExpr ex
  checkStmt (AST.SBlock stmts) = localScope $ AST.SBlock <$> traverse checkStmt stmts
  checkStmt (AST.SIf cond sthen selse) = AST.SIf <$> checkExpr cond <*> localScope (traverse checkStmt sthen) <*> localScope (traverse checkStmt selse)
  checkStmt (AST.SUntil cond body) = AST.SUntil <$> checkExpr cond <*> localScope (traverse checkStmt body)
  checkStmt (AST.SForEach ty var e body) = localScope $ do
    ty' <- checkType ty
    e' <- checkExpr e
    insertLocalVars [var]
    body' <- localScope (traverse checkStmt body)
    return $ AST.SForEach ty' var e' body'
  checkStmt (AST.SBreak) = pure AST.SBreak
  checkStmt (AST.SWait e) = AST.SWait <$> checkExpr e
  checkStmt (AST.SWaitUntil e) = AST.SWaitUntil <$> checkExpr e
  checkStmt (AST.SLock var e) = AST.SLock <$> checkTermName var <*> checkExpr e
  checkStmt (AST.SUnlock var) = AST.SUnlock <$> traverse checkTermName var
  checkStmt (AST.SOn cond body) = AST.SOn <$> globalScope (checkExpr cond) <*> traverse checkStmt body
  checkStmt (AST.SWhen cond body) = AST.SWhen <$> globalScope (checkExpr cond) <*> traverse checkStmt body

  checkBuiltin (AST.BuiltinStruct ssig) = AST.BuiltinStruct <$> checkStructSig ssig
  checkBuiltin (AST.BuiltinFun fsig) = AST.BuiltinFun <$> checkFunSig fsig
  checkBuiltin (AST.BuiltinVar vsig) = AST.BuiltinVar <$> checkVarSig vsig

  -- checks a function signature and introduces local generic type variables and parameters
  checkFunSig (AST.FunSig vis ret name gen params oname) = enterDecl name $ do
    insertGenerics gen
    ret' <- checkType ret
    checkOptParams params
    -- find duplicate parameter names
    let pnames = map (view AST.paramName) params
        pdupl = nub $ pnames \\ nub pnames
    forM_ pdupl $ \p -> messageWithContext MessageError $ MessageDuplicateParameter p
    params' <- mapM checkParam params
    insertLocalVars (map (view AST.paramName) params)
    return $ AST.FunSig vis ret' name gen params' oname

  -- checks whether all optional parameters occur at the end of the parameter list
  checkOptParams (p1 : p2 : ps) = do
    when (has (AST.paramOpt . _Just) p1 && has (AST.paramOpt . _Nothing) p2) $
      messageWithContext MessageError $ MessageMandatoryAfterOptional (p2 ^. AST.paramName)
    checkOptParams (p2 : ps)
  checkOptParams _ = pure ()

  -- checks a structure signature
  checkStructSig (AST.StructSig vis name gen deriv fields) = localScope $ enterDecl name $ do
    insertGenerics gen
    AST.StructSig vis name gen <$> traverse checkType deriv <*> traverse checkFieldSig fields

  checkFieldSig (AST.FieldFunSig fsig) = localScope $ AST.FieldFunSig <$> checkFunSig fsig
  checkFieldSig (AST.FieldVarSig vsig) = localScope $ AST.FieldVarSig <$> checkVarSig vsig
  checkFieldSig (AST.FieldIndexSig isig) = localScope $ AST.FieldIndexSig <$> checkIndexSig isig

  checkVarSig (AST.VarSig vis ty name access outputName) = enterDecl name $ AST.VarSig vis <$> checkType ty <*> pure name <*> pure access <*> pure outputName

  checkIndexSig (AST.IndexSig vis ret arg name access) = enterDecl "[]" $ AST.IndexSig vis <$> checkType ret <*> checkType arg <*> pure name <*> pure access

  -- | Saves the current scope and returns when the action is completed.
  localScope act = do
    oldEnv <- get
    r <- act
    put oldEnv
    return r

  -- | Temporarily switches to the global scope. The scope is returned to the local scope after the argument action returns.
  globalScope act = localScope $ do
    -- remove all local names
    termScope %= Map.filter (not . AST.isLocalName)
    typeScope %= Map.filter (not . AST.isLocalName)
    act

  insertGenerics = traverse_ $ \v -> typeScope . at (AST.RawName [v]) .= Just (AST.ScopedLocal v)
  insertLocalVars = traverse_ $ \v -> termScope . at (AST.RawName [v]) .= Just (AST.ScopedLocal v)

makeRecordSig :: AST.RecDecl name -> AST.StructSig name
makeRecordSig (AST.RecDecl vis name gens vars) = AST.StructSig vis name gens Nothing (map AST.FieldVarSig vars)
