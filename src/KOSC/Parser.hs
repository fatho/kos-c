module KOSC.Parser where

import Text.Trifecta
import Text.Parser.Combinators
import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashSet                as HS
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Parser.Token.Style
import           Text.Trifecta

import KOSC.AST

-- * Expression Parser

-- | Style for type/term variable and top-level binding identifiers.
varStyle :: CharParsing m => IdentifierStyle m
varStyle = IdentifierStyle
            { _styleName      = "identifier"
            , _styleStart     = letter
            , _styleLetter    = alphaNum <|> oneOf "_"
            , _styleReserved = HS.fromList
                [ "return", "record", "module", "import", "as", "unqualified", "builtin", "structure" ]
            , _styleHighlight = H.Identifier
            , _styleReservedHighlight = H.ReservedIdentifier
            }

opStyle :: TokenParsing m => IdentifierStyle m
opStyle = emptyOps
  { _styleReserved = HS.fromList ["+", "-", "*", "/"]
  }

opSym :: Op -> String
opSym o = case o of
  OpPlus -> "+"
  OpMinus -> "-"
  OpMult -> "*"
  OpDiv -> "/"

-- | Parses a variable identifier.
rawNameP :: (TokenParsing m, Monad m) => m RawName
rawNameP = RawName <$> sepBy1 (ident varStyle) (symbol "::")

-- | Parses a reserved identifier.
reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = reserve varStyle

-- | Parses a reserved operator.
reservedOp :: (TokenParsing m, Monad m) => String -> m ()
reservedOp = reserve opStyle


opTable :: (Monad m, TokenParsing m) => [[Operator m (Expr RawName)]]
opTable = [ map (entry AssocLeft) [OpMult, OpDiv]
          , map (entry AssocLeft) [OpPlus, OpMinus]
          ]
  where
    entry a o = Infix (binOp o <$ reservedOp (opSym o)) a
    binOp o x y = EOp x o y

exprP :: (TokenParsing m, Monad m) => m (Expr RawName)
exprP = buildExpressionParser opTable argP

argP :: (TokenParsing m, Monad m) => m (Expr RawName)
argP = choice [stringP, scalarP, unknownP, accessorChainP]

stringP :: (TokenParsing m) => m (Expr RawName)
stringP = EString <$> stringLiteral

scalarP :: (TokenParsing m) => m (Expr RawName)
scalarP = EScalar . either fromIntegral id <$> integerOrDouble

unknownP :: (TokenParsing m) => m (Expr RawName)
unknownP = EUnknown <$ symbol "?"

accessorChainP :: (TokenParsing m, Monad m) => m (Expr RawName)
accessorChainP = do
  var <- rawNameP
  let chain inner = do
        cur <- optional $ (EAccessor inner <$> (dot *> ident varStyle))
               <|> (EIndex inner <$> brackets exprP)
               <|> (ECall inner <$> option [] (angles (commaSep typeP)) <*> parens (commaSep exprP))
        case cur of
          Just next -> chain next
          Nothing -> return inner
  chain (EVar var)


testExpr :: String -> (Expr RawName)
testExpr str = case parseString (exprP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e -> e

-- * Statement Parser

stmtP :: (TokenParsing m, Monad m) => m (Stmt RawName)
stmtP = (choice [stmtReturnP, try stmtDeclP, try stmtAssignP, stmtExprP] <* semi) <|> stmtBlockP

stmtDeclP :: (TokenParsing m, Monad m) => m (Stmt RawName)
stmtDeclP = SDeclVar <$> typeP <*> ident varStyle <* symbol "=" <*> exprP

stmtAssignP :: (TokenParsing m, Monad m) => m (Stmt RawName)
stmtAssignP = SAssign <$> exprP <* symbol "=" <*> exprP

stmtExprP :: (TokenParsing m, Monad m) => m (Stmt RawName)
stmtExprP = SExpr <$> exprP

stmtReturnP :: (TokenParsing m, Monad m) => m (Stmt RawName)
stmtReturnP = SReturn <$> (reserved "return" *> exprP)

stmtBlockP :: (TokenParsing m, Monad m) => m (Stmt RawName)
stmtBlockP = SBlock <$> braces (many stmtP)

testStmt :: String -> (Stmt RawName)
testStmt str = case parseString (stmtP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e -> e

-- * Declaration Parser

moduleNameP :: (TokenParsing m, Monad m) => m ModuleName
moduleNameP = sepBy1 (ident varStyle) (symbol "::")

moduleP :: (TokenParsing m, Monad m) => m (Module RawName)
moduleP = Module <$> (reserved "module" *> moduleNameP <* symbol ";") <*> many declP

declP :: (TokenParsing m, Monad m) => m (Decl RawName)
declP = choice [DeclImport <$> importDeclP, DeclFun <$> funDeclP, DeclBuiltin <$> builtinDeclP]

importDeclP :: (TokenParsing m, Monad m) => m ImportDecl
importDeclP = ImportDecl <$> (reserved "import" *> moduleNameP)
  <*> optional (reserved "as" *> moduleNameP)
  <*> option False (True <$ reserved "unqualified")
  <* semi

visibilityP :: (TokenParsing m, Monad m) => m Visibility
visibilityP = choice [Public <$ reserved "public", Private <$ reserved "private" ]

funDeclP :: (TokenParsing m, Monad m) => m (FunDecl RawName)
funDeclP = FunDecl <$> option Public visibilityP
           <*> typeP  -- return type
           <*> ident varStyle -- name
           <*> option [] (angles $ commaSep $ ident varStyle) -- generic parameters
           <*> parens (commaSep paramP) -- parameters
           <*> braces (many stmtP) -- body

paramP :: (TokenParsing m, Monad m) => m (Param RawName)
paramP = Param <$> typeP <*> ident varStyle <*> optional (symbol "=" *> exprP)

typeP :: (TokenParsing m, Monad m) => m (Type RawName)
typeP = mk <$> rawNameP <*> optional (angles $ commaSep typeP) where
  mk v Nothing = TypeName v
  mk v (Just args) = TypeGeneric v args

builtinDeclP :: (TokenParsing m, Monad m) => m (BuiltinDecl RawName)
builtinDeclP = BuiltinDecl <$> (reserved "builtin" *> optional stringLiteral) <*> option Public visibilityP <*> builtinP

builtinP :: (TokenParsing m, Monad m) => m (Builtin RawName)
builtinP = (BuiltinStruct <$> builtinStructP) <|> (BuiltinFun <$> builtinFunP) where
  builtinStructP = BuiltinStructDef <$> (reserved "structure" *> ident varStyle)
                   <*> option [] (angles $ commaSep1 $ ident varStyle)
                   <*> optional (colon *> typeP)
                   -- TODO: add fields to builtin structures
                   <* braces whiteSpace
  builtinFunP = BuiltinFunDef <$> typeP <*> ident varStyle
    <*> option [] (angles $ commaSep $ ident varStyle) -- generic parameters
    <*> parens (commaSep paramP)
    <* semi


testModule :: String -> (Module RawName)
testModule str = case parseString (moduleP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e -> e
