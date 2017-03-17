{-# LANGUAGE TemplateHaskell #-}
module KOSC.AST where

import           Control.Lens

-- * AST

-- | A module name consists of a series of namespaces and the actual module name.
type ModuleName = [String]

type Ident = String

-- | A variable or function name
newtype RawName = RawName { rawNameParts :: [Ident] }
  deriving (Eq, Ord, Read, Show)

-- data QualifiedName
--   = QualifiedLocal Ident               -- ^ a local variable (i.e. declared as a parameter or inside a function body, never visible outside a module)
--   | QualifiedInternal Ident            -- ^ a reference to something declared on the top-level of the same module
--   | QualifiedExternal ModuleName Ident -- ^ a reference to something imported from another module
--   deriving (Eq, Ord, Read, Show)

-- | Visibility of a declaration in a module
data Visibility = Public | Private deriving (Read, Show)

data Module name = Module
  { _moduleName :: ModuleName
  , _declarations :: [Decl name]
  } deriving (Read, Show)

data Decl name
  = DeclImport ImportDecl
  | DeclFun (FunDecl name)
  | DeclBuiltin (BuiltinDecl name)
  deriving (Read, Show)

data ImportDecl = ImportDecl
  { _importModuleName  :: ModuleName       -- ^ fully qualified name of the imported module
  , _importAlias       :: Maybe ModuleName -- ^ an optional alias that can be used to refer to modules. multiple imports can have the same alias
  , _importUnqualified :: Bool             -- ^ whether it should be possible to refer to imported entities with an unqualified name
  }
  deriving (Read, Show)

data Type name
  = TypeName name           -- ^ normal named type (can be a type variable)
  | TypeGeneric name [Type name] -- ^ generic type instantiation
  | TypeFunction (Type name) [Type name] [Type name] -- ^ function type with parameters and optional parameters separated
  deriving (Read, Show)

data Param name = Param { _paramType :: Type name, _paramName :: Ident, _paramOpt :: Maybe (Expr name) }
  deriving (Read, Show)

data BuiltinDecl name = BuiltinDecl
  { _builtinRealName   :: Maybe String
  , _builtinVisibility :: Visibility
  , _builtinDef :: Builtin name
  } deriving (Read, Show)

data Builtin name = BuiltinStruct (BuiltinStructDef name) | BuiltinFun (BuiltinFunDef name)
  deriving (Read, Show)

data BuiltinStructDef name = BuiltinStructDef
  { _builtinStructName :: Ident
  , _builtinStructGenerics :: [Ident]
  , _builtinStructDeriv :: Maybe (Type name)
  }
  deriving (Read, Show)

data BuiltinFunDef name = BuiltinFunDef
  { _builtinFunReturnType :: Type name
  , _builtinFunName       :: Ident
  , _builtinFunGenerics   :: [Ident]
  , _builtinFunParams     :: [Param name]
  }
  deriving (Read, Show)

data FunDecl name = FunDecl
  { _visibility        :: Visibility
  , _returnType        :: Type name
  , _functionName      :: Ident
  , _genericParameters :: [Ident]
  , _parameters        :: [Param name]
  , _statements        :: [Stmt name]
  }
  deriving (Read, Show)

data Op = OpPlus | OpMinus | OpMult | OpDiv
  deriving (Read, Show)

-- | Expressions (distinguishing between lvalues and rvalues is done at a later stage).
data Expr name
  = EVar name
  | EAccessor (Expr name) Ident
  | EIndex (Expr name) (Expr name)
  | EOp (Expr name) Op (Expr name)
  | ECall (Expr name) [Type name] [(Expr name)]
  | EScalar Double
  | EString String
  | EUnknown -- ^ used as a placeholder in optional parameters of builtin declarations
  deriving (Read, Show)

-- | Statements
data Stmt name
  = SDeclVar (Type name) Ident (Expr name)
  | SAssign (Expr name) (Expr name)
  | SReturn (Expr name)
  | SExpr (Expr name) -- ^ useful for expressions with side-effects such as function calls
  | SBlock [Stmt name]
  deriving (Read, Show)

-- * Lenses

makeLenses ''Module
makeLenses ''FunDecl
makeLenses ''ImportDecl
makeLenses ''BuiltinDecl
makeLenses ''BuiltinStructDef
makeLenses ''BuiltinFunDef
makeLenses ''Param
