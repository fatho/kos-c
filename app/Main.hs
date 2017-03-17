{-# LANGUAGE LambdaCase #-}
module Main where

import           KOSC.Compiler

import           Data.Foldable
import           Data.Semigroup               ((<>))
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CmdLine = CmdLine
  { searchPath :: [FilePath]
  , mainModule :: FilePath
  }

cmdParser :: Parser CmdLine
cmdParser = CmdLine
  <$> many (strOption (long "libpath" <> short 'L' <> metavar "SEARCHPATH" <> help "Searches for modules in these directories."))
  <*> argument str (metavar "FILE" <> help "File containing the main module")

main :: IO ()
main = do
  let opts = info (cmdParser <**> helper)
             ( fullDesc
               <> progDesc "Compiles a kOS-C program into a kOS script."
               <> header "koscc - The kOS-C Compiler" )
  options <- execParser opts
  --curdir <- getCurrentDirectory
  (result, messages) <- compileFile (FileBasedCompilerOptions $ searchPath options) (mainModule options)
  forM_ messages $ \msg -> PP.displayIO stdout (PP.renderPretty 0.8 200 $ PP.pretty msg) >> putStrLn ""
  case result of
    Right prog -> print prog
    Left errDoc -> PP.displayIO stdout (PP.renderPretty 0.8 200 $ PP.pretty errDoc)
