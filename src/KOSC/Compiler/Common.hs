module KOSC.Compiler.Common
  ( KOSCCompilerT
  , KOSCMessage
  , enterModule
  , enterDecl
  , module KOSC.Compiler.Monad
  , module KOSC.Compiler.Errors
  ) where

import Text.PrettyPrint.ANSI.Leijen

import KOSC.Compiler.Errors
import KOSC.Compiler.Monad
import qualified KOSC.Language.AST as AST

type KOSCCompilerT = CompilerT MessageContent
type KOSCMessage = CompilerMessage MessageContent

enterModule :: MonadCompiler e m => AST.ModuleName -> m a -> m a
enterModule modName = pushContext $ text "In module:" <+> pretty modName

enterDecl :: MonadCompiler e m => AST.Ident -> m a -> m a
enterDecl declName = pushContext $ text "In declaration:" <+> pretty declName
