import qualified Paths_kos_c as Paths

import Test.Hspec
import System.FilePath
import System.Directory
import qualified Text.PrettyPrint.ANSI.Leijen as PP

import qualified KOSC.Language.AST as AST
import qualified KOSC.Compiler as Compiler
import qualified KOSC.Compiler.Errors as Errors
import KOSC.Compiler.Monad

-- | Path to the kosc-prelude library
koscPreludePath :: IO FilePath
koscPreludePath = do
  dataDir <- Paths.getDataDir
  return $ dataDir </> "kosc-prelude"


-- | Lists the contents of a directory recursively.
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  isDir <- doesDirectoryExist path
  if isDir then return . concat =<< traverse (listDirectoryRecursive . (path </>)) =<< listDirectory path
           else return [path]

-- | Builds a module importing all modules of the kosc-prelude library in order to type check all of them.
getPreludeModules :: IO [(AST.ModuleName, FilePath)]
getPreludeModules = do
  preludePath <- koscPreludePath
  kcFiles <- map (makeRelative preludePath) . filter ((== ".kc") . takeExtensions) <$> listDirectoryRecursive preludePath
  let moduleNames = map (AST.ModuleName . splitDirectories . dropExtensions) kcFiles
  return $ zip moduleNames kcFiles

-- | Asserts that a compiler action runs without error.
shouldRunWithoutErrors :: CompilerT Errors.MessageContent IO a -> Expectation
shouldRunWithoutErrors compilerAction = do
  (result, msgs) <- runCompilerT compilerAction
  case result of
    Left msg -> expectationFailure $ PP.displayS (PP.renderPretty 0.8 200 $ PP.vcat $ map PP.pretty (msg : msgs)) ""
    Right _ -> return ()

main :: IO ()
main = hspec $ do
  describe "kosc-prelude" $ do
    preludePath <- runIO $ koscPreludePath
    mods <- runIO $ getPreludeModules
    let imports = map (\(n, _) -> AST.DeclImport $ AST.ImportDecl n Nothing True) mods
        mainFun = AST.DeclFun $ AST.FunDecl (AST.FunSig AST.Public (AST.TypeGeneric (AST.RawName ["Void"]) []) "Main" [] [] Nothing) []
        testMod = AST.Module (AST.ModuleName ["PreludeTest"]) (imports ++ [mainFun])
        hooks = Compiler.CompilerHooks $ Compiler.fileSystemImportResolver [preludePath]
    it "compiles without errors" $
      shouldRunWithoutErrors (Compiler.compile hooks testMod)
