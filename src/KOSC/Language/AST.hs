{-# LANGUAGE TemplateHaskell #-}
module KOSC.Language.AST where

import           Control.Lens
import           Data.List
import           Data.Semigroup
import qualified Text.PrettyPrint.ANSI.Leijen as PP

-- * AST

-- | A module name consists of a series of namespaces and the actual module name.
newtype ModuleName = ModuleName { moduleNameParts :: [String] } deriving (Eq, Ord, Read, Show)

type Ident = String

-- | A variable or function name
newtype RawName = RawName { rawNameParts :: [Ident] }
  deriving (Eq, Ord, Read, Show)

-- | A fully qualified name.
data ScopedName
  = ScopedLocal Ident         -- ^ refers a local variable, always unambiguous due to shadowing
  | ScopedGlobal [Ident]      -- ^ refers to a global entity
  | ScopedAmbiguous [[Ident]] -- ^ an ambiguous name, only leads to an error when its actually used
  deriving (Eq, Ord, Read, Show)

-- | Visibility of a declaration in a module
data Visibility = Public | Private deriving (Eq, Ord, Read, Show)

-- | Accessibility of builtin variables or fields.
data Accessibility = Get | Set | GetOrSet deriving (Eq, Ord, Read, Show)

data Module name = Module
  { _moduleName   :: ModuleName
  , _declarations :: [Decl name]
  } deriving (Read, Show)

type RawModule = Module RawName

data Decl name
  = DeclImport ImportDecl
  | DeclFun (FunDecl name)
  | DeclVar (VarDecl name)
  | DeclRec (RecDecl name)
  | DeclBuiltin (Builtin name)
  deriving (Read, Show)

data ImportDecl = ImportDecl
  { _importModuleName  :: ModuleName       -- ^ fully qualified name of the imported module
  , _importAlias       :: Maybe ModuleName -- ^ an optional alias that can be used to refer to modules. multiple imports can have the same alias
  , _importUnqualified :: Bool             -- ^ whether it should be possible to refer to imported entities with an unqualified name
  }
  deriving (Read, Show)

data Type name
  = TypeGeneric name [Type name] -- ^ generic type instantiation (when there are zero arguments, this is just a normal type)
  | TypeFunction (Type name) [Type name] [Type name] -- ^ function type with parameters and optional parameters separated
  deriving (Eq, Ord, Read, Show)

-- | Describes a function parameter.
data Param name = Param { _paramType :: Type name, _paramName :: Ident, _paramOpt :: Maybe (Expr name) }
  deriving (Read, Show)

data Builtin name = BuiltinStruct (StructSig name) | BuiltinFun (FunSig name) | BuiltinVar (VarSig name)
  deriving (Read, Show)

-- | Signature of a field in a structure.
data FieldSig name = FieldFunSig (FunSig name) | FieldVarSig (VarSig name) | FieldIndexSig (IndexSig name)
  deriving (Read, Show)

data IndexSig name = IndexSig
  { _indexSigVisibility :: Visibility
  , _indexSigReturnType :: Type name
  , _indexSigIndexType  :: Type name
  , _indexSigIndexName  :: Ident
  , _indexSigAccess     :: Accessibility
  } deriving (Read, Show)

-- | Signature of a variable, consisting of its type, name, and accessibility
data VarSig name = VarSig
  { _varSigVisibility :: Visibility
  , _varSigType       :: Type name
  , _varSigName       :: Ident
  , _varSigAccess     :: Accessibility
  , _varSigOutputName :: Maybe String
  } deriving (Read, Show)

-- | Structure signature, consisting of its name, its super structure and its fields
data StructSig name = StructSig
  { _structSigVisibility :: Visibility
  , _structSigName       :: Ident
  , _structSigGenerics   :: [Ident]
  , _structSigSuper      :: Maybe (Type name)
  , _structSigFields     :: [FieldSig name]
  } deriving (Read, Show)

-- | Function signature, consisting of its visibility, a return type, possibly some generic arguments and parameters.
data FunSig name = FunSig
  { _funSigVisibility :: Visibility
  , _funSigReturnType :: Type name
  , _funSigName       :: Ident
  , _funSigGenerics   :: [Ident]
  , _funSigParameters :: [Param name]
  , _funSigOutputName :: Maybe String
  }
  deriving (Show, Read)

-- | Declaration of a function, consisting of a signature and the function body.
data FunDecl name = FunDecl
  { _funDeclSignature  :: FunSig name
  , _funDeclStatements :: [Stmt name]
  }
  deriving (Read, Show)

-- | Declaration of a global variable.
data VarDecl name = VarDecl
  { _varDeclSignature   :: VarSig name
  , _varDeclInitializer :: Expr name
  } deriving (Read, Show)

-- | Declaration of a record type
data RecDecl name = RecDecl
  { _recDeclVisibility :: Visibility
  , _recDeclName       :: Ident
  , _recDeclGenerics   :: [Ident]
  , _recDeclVars       :: [VarSig name]
  } deriving (Read, Show)

data BinOp = BinOpPlus | BinOpMinus | BinOpMult | BinOpDiv | BinOpPow
  | BinOpEq | BinOpLeq | BinOpLess | BinOpGreater | BinOpGeq | BinOpNeq
  | BinOpAnd | BinOpOr
  deriving (Eq, Ord, Read, Show, Enum, Bounded)

data UnOp = UnOpNegate | UnOpNot
  deriving (Eq, Ord, Show, Read)

-- | Expressions (distinguishing between lvalues and rvalues is done at a later stage).
data Expr name
  = EVar name
  | EAccessor (Expr name) (Maybe (Type name)) Ident
  | EIndex (Expr name) (Maybe (Type name)) (Expr name)
  | EOp (Expr name) BinOp (Expr name)
  | EUnOp UnOp (Expr name)
  | ECall (Expr name) [Type name] [(Expr name)]
  | EScalar Double
  | EString String
  | ERecordInit name [Type name] [(Ident, Expr name)]
  | EUnknown -- ^ used as a placeholder in optional parameters of builtin declarations
  | ECast (Type name) (Expr name)
  | ELambda [Param name] (Type name) [Stmt name]
  | EAt (Expr name) -- ^ used to indicate a function reference, this is inserted by the type checker and not part of the front-end syntax
  deriving (Read, Show)

-- | Statements
data Stmt name
  = SDeclVar (Type name) Ident (Expr name)
  | SAssign (Expr name) (Expr name)
  | SReturn (Maybe (Expr name))
  | SExpr (Expr name) -- ^ useful for expressions with side-effects such as function calls
  | SBlock [Stmt name]
  | SIf (Expr name) [Stmt name] [Stmt name]
  | SForEach (Type name) Ident (Expr name) [Stmt name]
  | SUntil (Expr name) [Stmt name]
  | SBreak
  | SWait (Expr name)
  | SWaitUntil (Expr name)
  | SLock name (Expr name)
  | SUnlock (Maybe name)
  | SOn (Expr name) [Stmt name]
  | SWhen (Expr name) [Stmt name]
  deriving (Read, Show)

-- * Lenses

makeLenses ''Module
makeLenses ''FunDecl
makeLenses ''VarDecl
makeLenses ''RecDecl
makeLenses ''ImportDecl
makeLenses ''FunSig
makeLenses ''VarSig
makeLenses ''IndexSig
makeLenses ''StructSig
makeLenses ''FieldSig
makeLenses ''Param

instance PP.Pretty ModuleName where
  pretty (ModuleName np) = PP.text $ intercalate "::" np

instance PP.Pretty RawName where
  pretty (RawName np) = PP.text $ intercalate "::" np

-- | The semigroup instance implements precedence and ambiguity rules of name scoping.
instance Semigroup ScopedName where
  _ <> loc@(ScopedLocal _) = loc -- ^ local variable always wins name resolution, last defined shadows previous definitions
  loc@(ScopedLocal _) <> _ = loc
  (ScopedGlobal g) <> (ScopedGlobal g') = if g == g' then ScopedGlobal g else ScopedAmbiguous [g, g']
  (ScopedAmbiguous a) <> (ScopedGlobal g) = ScopedAmbiguous (g : a)
  (ScopedGlobal g) <> (ScopedAmbiguous a) = ScopedAmbiguous (g : a)
  (ScopedAmbiguous g1) <> (ScopedAmbiguous g2) = ScopedAmbiguous (g1 ++ g2)

instance PP.Pretty ScopedName where
  pretty (ScopedGlobal np) = PP.text $ intercalate "::" np
  pretty (ScopedLocal loc) = PP.text loc
  pretty (ScopedAmbiguous np) = PP.hcat (PP.punctuate (PP.comma PP.<> PP.space) (map (PP.text . intercalate "::") np))

instance PP.Pretty name => PP.Pretty (Type name) where
  pretty (TypeGeneric n args) = PP.pretty n PP.<> if not (null args) then PP.encloseSep PP.langle PP.rangle PP.comma (map PP.pretty args) else PP.empty
  pretty (TypeFunction ret args opts) = PP.pretty ret PP.<> PP.tupled (map PP.pretty args) PP.<> PP.list (map PP.pretty opts)

-- * Helper Functions

isOptional :: Param name -> Bool
isOptional = has $ paramOpt . _Just

isMandatory :: Param name -> Bool
isMandatory = not . isOptional

makeGlobalName :: ModuleName -> Ident -> ScopedName
makeGlobalName modName ident = ScopedGlobal (moduleNameParts modName ++ [ident])

makeRawName :: ModuleName -> Ident -> RawName
makeRawName modName ident = RawName (moduleNameParts modName ++ [ident])

isLocalName :: ScopedName -> Bool
isLocalName (ScopedLocal _) = True
isLocalName _ = False
