{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
module KOSC.Compiler.Errors where

import           Data.List
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
  | MessageMandatoryAfterOptional AST.Ident
  | MessageDuplicateParameter AST.Ident
  | MessageTypesNotEqual (AST.Type AST.ScopedName) (AST.Type AST.ScopedName)
  | MessageFunctionExpected (AST.Type AST.ScopedName)
  | MessageGenericTypeMismatch
  | MessageNotEnoughArguments
  | MessageTooManyArguments
  | MessageFieldNotFound (AST.Type AST.ScopedName) AST.Ident
  | MessageWrongAccessibility AST.Accessibility
  | MessageUninitializedIdentifiers [AST.Ident]
  deriving (Show)

instance PP.Pretty MessageContent where
  pretty (MessageDuplicateDeclaration def) = squotes (text def) <+> text " is defined more than once."
  pretty (MessageNotInScope n) = PP.text "Identifier" <+> pretty n <+> text "not in scope"
  pretty (MessageAmbiguousName n others) = text "Identifier" <+> pretty n <+> text "is ambiguous. It could refer to" <+> pretty others
  pretty (MessageModuleNotFound modname) = text "Module" <+> PP.squotes (pretty modname) <+> text " not found."
  pretty (MessageCannotProceedWithErrors) = text "Cannot proceed when there are errors."
  pretty (MessageUnspecified doc) = doc
  pretty (MessageInvalidModuleDeclaration declared imported) = PP.text "Module declared as " PP.<+> PP.squotes (PP.pretty declared) PP.<+> PP.text "but imported as" PP.<+> PP.squotes (PP.pretty imported)
  pretty (MessageMandatoryAfterOptional nonopt) = text "Mandatory parameter" <+> squotes (text nonopt) <+> text "must precede all optional parameters."
  pretty (MessageDuplicateParameter param) = text "Duplicate parameter" <+> squotes (text param)
  pretty (MessageTypesNotEqual t1 t2) = text "Expected type" <+> squotes (pretty t1) <+> text "but the actual type is" <+> squotes (pretty t2)
  pretty (MessageFunctionExpected smthelse) = text "Expected a function but got something else:" <+> pretty smthelse
  pretty (MessageGenericTypeMismatch) = text "Number of generic parameters does not match."
  pretty (MessageNotEnoughArguments) = text "Not enough arguments provided to function call."
  pretty (MessageTooManyArguments) = text "Too many arguments provided to function call."
  pretty (MessageFieldNotFound ty field) = text "Type" <+> squotes (pretty ty) <+> text "has no field" <+> squotes (pretty field)
  pretty (MessageWrongAccessibility acc) = case acc of
    AST.Get -> pretty "Expression must be gettable, but is only settable."
    AST.Set -> pretty "Expression must be settable, but is only gettable."
    AST.GetOrSet -> pretty "Expression must be both gettable and settable."
  pretty (MessageUninitializedIdentifiers args) = text "The following identifiers have not been initialized:" <+> text (intercalate ", " args)
