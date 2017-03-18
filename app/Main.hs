{-# LANGUAGE LambdaCase #-}
module Main where

import           KOSC.Compiler

import           Data.Foldable
import           Data.Semigroup               ((<>))
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.IO
import qualified Data.Text.Lazy.IO as L
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CmdLine = CmdLine
  { searchPath :: [FilePath]
  , outputFile :: Maybe FilePath
  , mainModule :: FilePath
  }

cmdParser :: Parser CmdLine
cmdParser = CmdLine
  <$> many (strOption (long "libpath" <> short 'L' <> metavar "SEARCHPATH" <> help "Searches for modules in these directories."))
  <*> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "The generated code is written to this file. If omitted, it is printed to stdout instead."))
  <*> argument str (metavar "FILE" <> help "File containing the main module")

main :: IO ()
main = do
  let opts = info (cmdParser <**> helper)
             ( fullDesc
               <> progDesc "Compiles a kOS-C program into a kOS script."
               <> header "koscc - The kOS-C Compiler" )
  options <- execParser opts
  (result, messages) <- compileFile (FileBasedCompilerOptions $ searchPath options) (mainModule options)
  forM_ messages $ \msg -> PP.displayIO stderr (PP.renderPretty 0.8 200 $ PP.pretty msg) >> putStrLn ""
  case result of
    Right prog -> case outputFile options of
      Nothing -> L.putStrLn prog
      Just f -> L.writeFile f prog
    Left errDoc -> PP.displayIO stderr (PP.renderPretty 0.8 200 $ PP.pretty errDoc)
