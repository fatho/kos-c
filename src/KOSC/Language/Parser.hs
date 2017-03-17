module KOSC.Language.Parser where

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

import KOSC.Language.AST

-- * Expression Parser

-- | Style for type/term variable and top-level binding identifiers.
varStyle :: CharParsing m => IdentifierStyle m
varStyle = IdentifierStyle
            { _styleName      = "identifier"
            , _styleStart     = letter
            , _styleLetter    = alphaNum <|> oneOf "_"
            , _styleReserved = HS.fromList
                [ "return", "record", "module", "import", "as", "unqualified", "builtin", "structure", "get", "set" ]
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
rawNameP ::  Parser RawName
rawNameP = RawName <$> sepBy1 (ident varStyle) (symbol "::")

-- | Parses a reserved identifier.
reserved ::  String -> Parser ()
reserved = reserve varStyle

-- | Parses a reserved operator.
reservedOp ::  String -> Parser ()
reservedOp = reserve opStyle

opTable :: [[Operator Parser (Expr RawName)]]
opTable = [ map (entry AssocLeft) [OpMult, OpDiv]
          , map (entry AssocLeft) [OpPlus, OpMinus]
          ]
  where
    entry a o = Infix (binOp o <$ reservedOp (opSym o)) a
    binOp o x y = EOp x o y

exprP ::  Parser (Expr RawName)
exprP = buildExpressionParser opTable argP

argP ::  Parser (Expr RawName)
argP = choice [stringP, scalarP, unknownP, accessorChainP]

stringP ::  Parser (Expr RawName)
stringP = EString <$> stringLiteral

scalarP ::  Parser (Expr RawName)
scalarP = EScalar . either fromIntegral id <$> integerOrDouble

unknownP ::  Parser (Expr RawName)
unknownP = EUnknown <$ symbol "?"

accessorChainP ::  Parser (Expr RawName)
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

stmtP ::  Parser (Stmt RawName)
stmtP = (choice [stmtReturnP, try stmtDeclP, try stmtAssignP, stmtExprP] <* semi) <|> stmtBlockP

stmtDeclP ::  Parser (Stmt RawName)
stmtDeclP = SDeclVar <$> typeP <*> ident varStyle <* symbol "=" <*> exprP

stmtAssignP ::  Parser (Stmt RawName)
stmtAssignP = SAssign <$> exprP <* symbol "=" <*> exprP

stmtExprP ::  Parser (Stmt RawName)
stmtExprP = SExpr <$> exprP

stmtReturnP ::  Parser (Stmt RawName)
stmtReturnP = SReturn <$> (reserved "return" *> exprP)

stmtBlockP ::  Parser (Stmt RawName)
stmtBlockP = SBlock <$> braces (many stmtP)

testStmt :: String -> (Stmt RawName)
testStmt str = case parseString (stmtP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e -> e

-- * Declaration Parser

moduleNameP ::  Parser ModuleName
moduleNameP = ModuleName <$> sepBy1 (ident varStyle) (symbol "::")

moduleP ::  Parser (Module RawName)
moduleP = Module <$> (reserved "module" *> moduleNameP <* symbol ";") <*> many declP

declP ::  Parser (Decl RawName)
declP = choice [DeclImport <$> importDeclP, DeclFun <$> funDeclP, DeclBuiltin <$> builtinP]

importDeclP ::  Parser ImportDecl
importDeclP = ImportDecl <$> (reserved "import" *> moduleNameP)
  <*> optional (reserved "as" *> moduleNameP)
  <*> option False (True <$ reserved "unqualified")
  <* semi

visibilityP ::  Parser Visibility
visibilityP = choice [Public <$ reserved "public", Private <$ reserved "private" ]

funSigP ::  Parser (FunSig RawName)
funSigP = FunSig <$> option Public visibilityP
           <*> typeP  -- return type
           <*> ident varStyle -- name
           <*> option [] (angles $ commaSep $ ident varStyle) -- generic parameters
           <*> parens (commaSep paramP) -- parameters

varSigP ::  Parser (VarSig RawName)
varSigP = VarSig <$> option Public visibilityP
           <*> typeP
           <*> ident varStyle
           <*> accessibilityP


indexSigP ::  Parser (IndexSig RawName)
indexSigP = IndexSig <$> option Public visibilityP
           <*> typeP
           <* symbol "[" <*> typeP <*> ident varStyle <* symbol "]"
           <*> accessibilityP

fieldSigP ::  Parser (FieldSig RawName)
fieldSigP = (FieldFunSig <$> try funSigP) <|> (FieldVarSig <$> try varSigP) <|> (FieldIndexSig <$> indexSigP)

structSigP ::  Parser (StructSig RawName)
structSigP = StructSig <$> option Public visibilityP
  <*> (reserved "structure" *> ident varStyle)
  <*> option [] (angles $ commaSep1 $ ident varStyle)
  <*> optional (colon *> typeP)
  <*> braces (many (fieldSigP <* semi))

funDeclP ::  Parser (FunDecl RawName)
funDeclP = FunDecl <$> funSigP <*> braces (many stmtP) -- body

paramP ::  Parser (Param RawName)
paramP = Param <$> typeP <*> ident varStyle <*> optional (symbol "=" *> exprP)

typeP ::  Parser (Type RawName)
typeP = mk <$> rawNameP <*> optional (angles $ commaSep typeP) where
  mk v Nothing = TypeName v
  mk v (Just args) = TypeGeneric v args

accessibilityP ::  Parser Accessibility
accessibilityP = option GetOrSet $ (Get <$ reserved "get") <|> (Set <$ reserved "set")

builtinP ::  Parser (Builtin RawName)
builtinP = reserved "builtin" >> (BuiltinStruct <$> structSigP) <|> (BuiltinFun <$> funSigP) <|> (BuiltinVar <$> varSigP)

testModule :: String -> (Module RawName)
testModule str = case parseString (moduleP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e -> e
