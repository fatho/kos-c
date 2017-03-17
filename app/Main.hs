{-# LANGUAGE LambdaCase #-}
module Main where

import           KOSC.Compiler

import           Data.Semigroup               ((<>))
import           Options.Applicative
import           System.Environment
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as PP

data CmdLine = CmdLine
  { mainModule :: FilePath
  }

cmdParser :: Parser CmdLine
cmdParser = CmdLine
  <$> argument str (metavar "FILE" <> help "File containing the main module")

main :: IO ()
main = do
  let opts = info (cmdParser <**> helper)
             ( fullDesc
               <> progDesc "Compiles a kOS-C program into a kOS script."
               <> header "koscc - The kOS-C Compiler" )
  options <- execParser opts
  compileFile (mainModule options) >>= \case
    Right prog -> print prog
    Left errDoc -> PP.displayIO stdout (PP.renderPretty 0.8 200 errDoc)
