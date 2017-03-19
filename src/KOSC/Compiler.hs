{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-| Main entry point of the compiler. -}
module KOSC.Compiler where

import           Control.Lens
import           Control.Monad.Except
import           Data.List
import           System.Directory
import           System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen   as PP
import qualified Text.Trifecta                  as Trifecta

import qualified KOSC.Language.AST              as AST
import qualified KOSC.Language.Parser           as Parser

import           KOSC.Compiler.Common
import           KOSC.Compiler.ImportResolution
import           KOSC.Compiler.ScopeChecker
import           KOSC.Compiler.TypeChecker
import           KOSC.Compiler.CodeGen

data CompilerHooks m = CompilerHooks
  { resolveImport :: AST.ModuleName -> KOSCCompilerT m AST.RawModule
  }

-- | Compiles a program given by a main module and returns the corresponding kOS Script code.
compile :: Monad m => CompilerHooks m -> AST.Module AST.RawName -> KOSCCompilerT m GeneratedCode
compile hooks mainModule = do
  imports <- resolveImports (resolveImport hooks) mainModule
  cancelIfErrorneous $ MessageUnspecified $ PP.text "Failed to resolve imports."
  scopedMods <- forM (imports ^. importResolutionModules) $
                \mod -> scopeChecker imports (mod ^. moduleInfoAST)
  cancelIfErrorneous $ MessageUnspecified $ PP.text "Cannot type check without successfully completing scope checking."
  typedMods <- forM scopedMods $ \mod -> typeChecker scopedMods mod
  cancelIfErrorneous $ MessageUnspecified $ PP.text "Cannot generate code with errors."
  codeGen typedMods (view AST.moduleName mainModule)

-- * File Based Compiler

data FileBasedCompilerOptions = FileBasedCompilerOptions
  { _librarySearchPath :: [FilePath]
  -- ^ the directories where to look for modules
  }
makeLenses ''FileBasedCompilerOptions

-- | Parses a module file and uses the compiler monad for handling errors.
parseModuleFile :: FilePath -> KOSCCompilerT IO (AST.Module AST.RawName)
parseModuleFile filename = do
  modparse <- Trifecta.parseFromFileEx (Parser.runKOSCParser $ Parser.moduleP <* Trifecta.eof) filename
  case modparse of
    Trifecta.Failure (Trifecta.ErrInfo doc _) -> do
      criticalWithContext (MessageUnspecified doc)
    Trifecta.Success mod                      -> return mod

-- | Resolves module import in the file system by searching the given library paths.
fileSystemImportResolver :: [FilePath] -> AST.ModuleName -> KOSCCompilerT IO (AST.Module AST.RawName)
fileSystemImportResolver searchPath modName = do
  let modPathRel = foldl1' (</>) (AST.moduleNameParts modName) ++ ".kc"
      modPaths = map (</> modPathRel) searchPath
      search [] = do
        messageWithContext MessageError (MessageModuleNotFound modName)
        -- return dummy module and keep accumulating errors
        return (AST.Module modName [])
      search (p:ps) = liftIO (doesFileExist p) >>= \case
        True -> parseModuleFile p
        False -> search ps
  mod <- search modPaths
  let declaredName = view AST.moduleName mod
  when (declaredName /= modName) $
    messageWithContext MessageError (MessageInvalidModuleDeclaration declaredName modName)
  return mod

-- | Compiles a program from a file.
compileFile :: FileBasedCompilerOptions -> FilePath -> IO (Either KOSCMessage GeneratedCode, [KOSCMessage])
compileFile opts filename = runCompilerT $ do
  mainmod <- parseModuleFile filename
  let hooks = CompilerHooks { resolveImport = fileSystemImportResolver (opts ^. librarySearchPath) }
  compile hooks mainmod


