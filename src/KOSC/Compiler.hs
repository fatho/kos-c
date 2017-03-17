{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-| Main entry point of the compiler. -}
module KOSC.Compiler where

import           Control.Lens
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

import qualified KOSC.AST                     as AST
import qualified KOSC.Parser                  as Parser

import Debug.Trace

data CompiledModule = CompiledModule
  { _rawAST                  :: AST.Module AST.RawName
  , _scopedAST               :: AST.Module ScopedName
  }
  deriving (Read, Show)

data CompiledProgram = CompiledProgram
  { _mainModule   :: CompiledModule
  , _otherModules :: Map.Map AST.ModuleName CompiledModule
  }
  deriving (Read, Show)

data CompilerHooks m = CompilerHooks
  { resolveImport :: AST.ModuleName -> m (AST.Module AST.RawName)
  }

data ScopedName
  = ScopedLocal AST.Ident    -- ^ refers a local variable, always unambiguous due to shadowing
  | ScopedGlobal [AST.Ident] -- ^ refers to a global entity
  | ScopedAmbiguous [[AST.Ident]] -- ^ an ambiguous name, only leads to an error when its actually used
  deriving (Eq, Ord, Read, Show)

instance Semigroup ScopedName where
  _ <> loc@(ScopedLocal _) = loc -- ^ local variable always wins name resolution, last defined shadows previous definitions
  loc@(ScopedLocal _) <> _ = loc
  (ScopedGlobal g) <> (ScopedGlobal g') = if g == g' then ScopedGlobal g else ScopedAmbiguous [g, g']
  (ScopedAmbiguous a) <> (ScopedGlobal g) = ScopedAmbiguous (g : a)
  (ScopedGlobal g) <> (ScopedAmbiguous a) = ScopedAmbiguous (g : a)

type CompilerError = PP.Doc

-- | Internal state of the compiler
data CompilerState = CompilerState
  { _importedModules   :: Map AST.ModuleName (AST.Module AST.RawName) -- ^ caching the AST of imported modules, so that only one copy is loaded for each module
  , _scopeCheckedModules :: Map AST.ModuleName (AST.Module ScopedName)
  , _exportedTermNames :: Map AST.ModuleName (Set AST.Ident) -- ^ names of exported term level things
  , _exportedTypeNames :: Map AST.ModuleName (Set AST.Ident) -- ^ names of exported type level things
  }

makeLenses ''CompilerState

-- | Environment of the scope checker.
data ScopeEnv = ScopeEnv
  { _termScope :: Map AST.RawName ScopedName -- ^ unqualified term identifiers in scope
  , _typeScope :: Map AST.RawName ScopedName -- ^ unqualified type identifiers in scope
  }

makeLenses ''ScopeEnv

instance Monoid ScopeEnv where
  mempty = ScopeEnv Map.empty Map.empty
  mappend (ScopeEnv te1 ty1) (ScopeEnv te2 ty2) = ScopeEnv (te1 <> te2) (ty1 <> ty2)

-- | @getImportList m@ returns the list of modules that are imported by @m@.
getImportList :: AST.Module name -> [AST.ModuleName]
getImportList mod = [ name | AST.DeclImport (AST.ImportDecl name _ _) <- view AST.declarations mod ]

-- | @getExportedTermNames m@ returns a set of exported term-level names, i.e. functions and global variables.
getExportedTermNames :: AST.Module name -> [AST.Ident]
getExportedTermNames mod = [ name | decl <- view AST.declarations mod, name <- getname decl ] where
  getname (AST.DeclImport _) = []
  getname (AST.DeclFun fd) = [view AST.functionName fd]
  getname (AST.DeclBuiltin (AST.BuiltinDecl _ _ (AST.BuiltinStruct def))) = [] -- struct is only a type name
  getname (AST.DeclBuiltin (AST.BuiltinDecl _ _ (AST.BuiltinFun def))) = [view AST.builtinFunName def]

-- | @getExportedTermNames m@ returns a set of exported type-level names (for now nothing, but will be records)
getExportedTypeNames :: AST.Module name -> [AST.Ident]
getExportedTypeNames mod = [ name | decl <- view AST.declarations mod, name <- getname decl ] where
  getname (AST.DeclImport _) = []
  getname (AST.DeclFun fd) = []
  getname (AST.DeclBuiltin (AST.BuiltinDecl _ _ (AST.BuiltinStruct def))) = [view AST.builtinStructName def]
  getname (AST.DeclBuiltin (AST.BuiltinDecl _ _ (AST.BuiltinFun def))) = []

-- | Compiles a program given by a main module.
compile :: MonadError CompilerError m => CompilerHooks m -> AST.Module AST.RawName -> m CompiledProgram
compile hooks mainModule = evalStateT go initialState where
  mainModuleName = view AST.moduleName mainModule

  initialState = CompilerState
    { _importedModules = Map.empty
    , _scopeCheckedModules = Map.empty
    , _exportedTermNames = Map.empty
    , _exportedTypeNames = Map.empty
    }

  resolveImports [] = return ()
  resolveImports (name:rest) = use (importedModules . at name) >>= \case
    Nothing -> do
      mod <- if name == mainModuleName then pure mainModule else lift $ resolveImport hooks name
      importedModules . at name .= Just mod
      let expTerms = getExportedTermNames mod
          expTypes = getExportedTypeNames mod
          expTermSet = Set.fromList expTerms
          expTypeSet = Set.fromList expTypes
          duplTerms = expTerms \\ Set.toList expTermSet
          duplTypes = expTypes \\ Set.toList expTypeSet
      when (not $ null duplTerms) $ throwError $ PP.text $ "Duplicate definitions of terms " ++ intercalate ", " duplTerms ++ " in " ++ intercalate "::" name
      when (not $ null duplTypes) $ throwError $ PP.text $ "Duplicate definitions of types " ++ intercalate ", " duplTypes ++ " in " ++ intercalate "::" name
      exportedTermNames . at name .= Just expTermSet
      exportedTypeNames . at name .= Just expTypeSet
      resolveImports $ getImportList mod ++ rest
    Just _ -> return ()

  go = do
    -- first, resolve imports transitively, populating the module cache and export lists
    resolveImports [mainModuleName]
    -- now, scope check every module separately
    use importedModules >>= traverse scopeChecker >>= assign scopeCheckedModules
    -- TODO: perform type checking
    imp <- use importedModules
    sc <- use scopeCheckedModules
    return $ CompiledProgram (CompiledModule mainModule (sc Map.! mainModuleName)) (fmap (\raw -> CompiledModule raw (sc Map.! view AST.moduleName raw)) imp)

-- | Resolves all references in a module to module-qualified names
scopeChecker :: MonadError CompilerError m => AST.Module AST.RawName -> StateT CompilerState m (AST.Module ScopedName)
scopeChecker inputMod = initialScope >>= evalStateT checkedMod where
  initialScope = do
    -- list of all imported modules, and the current module itself
    let imported = (AST.ImportDecl (view AST.moduleName inputMod) Nothing True)
                   : [ decl | AST.DeclImport decl <- view AST.declarations inputMod ]
    -- build a scope for all imports
    fold <$> traverse importedScope imported

  importedScope importDecl = do
    let importedName = view AST.importModuleName importDecl
    -- get exported term and type names of the given module
    defTerms <- uses (exportedTermNames . ix importedName) Set.toList
    defTypes <- uses (exportedTypeNames . ix importedName) Set.toList
    -- create scope maps
    let qualName = fromMaybe importedName (view AST.importAlias importDecl)
        -- use alias name, if given
        qualTermMap = Map.fromList [ (AST.RawName (qualName ++ [n]), ScopedGlobal (importedName ++ [n])) | n <- defTerms ]
        qualTypeMap = Map.fromList [ (AST.RawName (qualName ++ [n]), ScopedGlobal (importedName ++ [n])) | n <- defTypes ]
        -- also import unqualified, if requested
        unqualified = view AST.importUnqualified importDecl
        uqualTermMap = if unqualified then Map.fromList [ (AST.RawName [n], ScopedGlobal (importedName ++ [n])) | n <- defTerms ] else Map.empty
        uqualTypeMap = if unqualified then Map.fromList [ (AST.RawName [n], ScopedGlobal (importedName ++ [n])) | n <- defTypes ] else Map.empty
    return $ ScopeEnv (Map.unionWith (<>) qualTermMap uqualTermMap) (Map.unionWith (<>) qualTypeMap uqualTypeMap)

  checkedMod =
    mapMOf (AST.declarations . traversed) checkDecl inputMod

  checkDecl (AST.DeclImport i) = pure (AST.DeclImport i)
  checkDecl (AST.DeclFun fd) = AST.DeclFun <$> checkFun fd
  checkDecl (AST.DeclBuiltin bi) = AST.DeclBuiltin <$> checkBuiltinDecl bi

  checkFun (AST.FunDecl vis rt name gen params stmts) = localScope $ do
    insertGenerics gen
    rt' <- checkType rt
    params' <- mapM checkParam params
    insertLocalVars (map (view AST.paramName) params)
    stmts' <- mapM checkStmt stmts
    return (AST.FunDecl vis rt' name gen params' stmts')

  checkName desc mapLens n = do
    scope <- use mapLens
    case Map.lookup n scope of
      Nothing -> do
        throwError $ PP.text $ desc ++ " " ++ intercalate "::" (AST.rawNameParts n) ++ " not in scope " ++ show scope
      Just (ScopedAmbiguous a) -> throwError $ PP.text $ desc ++ " " ++ intercalate "::" (AST.rawNameParts n) ++ " is ambiguous. It could refer to " ++ intercalate ", " (map (intercalate "::") a)
      Just fine -> pure fine

  checkTypeName = checkName "Type" typeScope

  checkTermName = checkName "Variable" termScope

  checkType (AST.TypeName n) = AST.TypeName <$> checkTypeName n
  checkType (AST.TypeGeneric n args) = AST.TypeGeneric <$> checkTypeName n <*> traverse checkType args
  checkType (AST.TypeFunction ret args opts) = AST.TypeFunction <$> checkType ret <*> traverse checkType args <*> traverse checkType opts

  checkParam (AST.Param ty name expr) = AST.Param <$> checkType ty <*> pure name <*> traverse checkExpr expr

  checkExpr (AST.EVar v) = AST.EVar <$> checkTermName v
  checkExpr (AST.EAccessor e field) = AST.EAccessor <$> checkExpr e <*> pure field
  checkExpr (AST.EIndex e eidx) = AST.EIndex <$> checkExpr e <*> checkExpr eidx
  checkExpr (AST.EOp e1 op e2) = AST.EOp <$> checkExpr e1 <*> pure op <*> checkExpr e2
  checkExpr (AST.ECall f tyargs args) = AST.ECall <$> checkExpr f <*> traverse checkType tyargs <*> traverse checkExpr args
  checkExpr (AST.EScalar d) = pure $ AST.EScalar d
  checkExpr (AST.EString s) = pure $ AST.EString s

  checkStmt (AST.SDeclVar ty name init) = do
    insertLocalVars [name]
    AST.SDeclVar <$> checkType ty <*> pure name <*> checkExpr init
  checkStmt (AST.SAssign lhs rhs) = AST.SAssign <$> checkExpr lhs <*> checkExpr rhs
  checkStmt (AST.SReturn ret) = AST.SReturn <$> checkExpr ret
  checkStmt (AST.SExpr ex) = AST.SExpr <$> checkExpr ex
  checkStmt (AST.SBlock stmts) = localScope $ AST.SBlock <$> traverse checkStmt stmts

  checkBuiltinDecl (AST.BuiltinDecl vis name what) = AST.BuiltinDecl vis name <$> checkBuiltin what

  checkBuiltin (AST.BuiltinStruct sdef) = AST.BuiltinStruct <$> checkBuiltinStruct sdef
  checkBuiltin (AST.BuiltinFun fdef) = AST.BuiltinFun <$> checkBuiltinFun fdef

  checkBuiltinStruct (AST.BuiltinStructDef name gen deriv) = localScope $ do
    insertGenerics gen
    AST.BuiltinStructDef name gen <$> traverse checkType deriv

  checkBuiltinFun (AST.BuiltinFunDef ret name gen params) = localScope $ do
    insertGenerics gen
    AST.BuiltinFunDef <$> checkType ret <*> pure name <*> pure gen <*> traverse checkParam params

  localScope act = do
    oldEnv <- get
    r <- act
    put oldEnv
    return r

  insertGenerics = traverse_ $ \v -> typeScope . at (AST.RawName [v]) .= Just (ScopedLocal v)
  insertLocalVars = traverse_ $ \v -> termScope . at (AST.RawName [v]) .= Just (ScopedLocal v)

-- * File Based Compiler

parseModuleFile :: FilePath -> ExceptT CompilerError IO (AST.Module AST.RawName)
parseModuleFile filename = do
  modparse <- Trifecta.parseFromFileEx (Parser.moduleP <* Trifecta.eof) filename
  case modparse of
    Trifecta.Failure (Trifecta.ErrInfo doc _) -> throwError doc
    Trifecta.Success mod                      -> return mod

fileSystemImportResolver :: FilePath -> AST.ModuleName -> ExceptT CompilerError IO (AST.Module AST.RawName)
fileSystemImportResolver basePath modName = do
  let fn = foldl' (</>) basePath modName
  mod <- parseModuleFile $ fn ++ ".kc"
  let declaredName = view AST.moduleName mod
  if declaredName /= modName
    then throwError $ PP.text $ "Module declared as '" ++ intercalate "::" declaredName ++ "' but imported as '" ++ intercalate "::" modName ++ "'"
    else return mod

-- | Compiles a program from a file.
compileFile :: FilePath -> IO (Either CompilerError CompiledProgram)
compileFile filename = runExceptT $ do
  mainmod <- parseModuleFile filename
  basePath <- liftIO $ getCurrentDirectory
  let hooks = CompilerHooks { resolveImport = fileSystemImportResolver basePath }
  compile hooks mainmod


