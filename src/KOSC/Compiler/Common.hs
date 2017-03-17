module KOSC.Compiler.Common
  ( KOSCCompilerT
  , KOSCMessage
  , module KOSC.Compiler.Monad
  , module KOSC.Compiler.Errors
  ) where

import KOSC.Compiler.Errors
import KOSC.Compiler.Monad

type KOSCCompilerT = CompilerT MessageContent
type KOSCMessage = CompilerMessage MessageContent
