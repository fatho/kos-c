{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
module KOSC.Compiler.Errors where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe
import           Data.Monoid

import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))

import qualified KOSC.Language.AST            as AST

-- * Error messages

-- | An abstract type for compiler error messages
data MessageContent
  = MessageDuplicateDeclaration AST.Ident
  | MessageNotInScope AST.RawName
  | MessageAmbiguousName AST.RawName AST.ScopedName
  | MessageModuleNotFound AST.ModuleName
  | MessageCannotProceedWithErrors
  | MessageUnspecified PP.Doc
  | MessageInvalidModuleDeclaration AST.ModuleName AST.ModuleName
  deriving (Show)

instance PP.Pretty MessageContent where
  pretty (MessageDuplicateDeclaration def) = squotes (text def) <+> text " is defined more than once."
  pretty (MessageNotInScope n) = PP.text "Identifier" <+> pretty n <+> text "not in scope"
  pretty (MessageAmbiguousName n others) = text "Identifier" <+> pretty n <+> text "is ambiguous. It could refer to" <+> pretty others
  pretty (MessageModuleNotFound modname) = text "Module" <+> PP.squotes (pretty modname) <+> text " not found."
  pretty (MessageCannotProceedWithErrors) = text "Cannot proceed when there are errors."
  pretty (MessageUnspecified doc) = doc
  pretty (MessageInvalidModuleDeclaration declared imported) = PP.text "Module declared as " PP.<+> PP.squotes (PP.pretty declared) PP.<+> PP.text "but imported as" PP.<+> PP.squotes (PP.pretty imported)
