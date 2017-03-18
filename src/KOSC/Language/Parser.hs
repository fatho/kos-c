{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module KOSC.Language.Parser where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import qualified Data.HashSet                as HS
import           Text.Parser.Combinators
import           Text.Parser.Expression
import qualified Text.Parser.Token.Highlight as H
import           Text.Parser.Token.Style
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Parser.LookAhead

import           KOSC.Language.AST

-- * Expression Parser

newtype KOSCParser a = KOSCParser { runKOSCParser :: Parser a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Parsing, CharParsing, Errable, DeltaParsing, LookAheadParsing, MarkParsing Delta)

instance TokenParsing KOSCParser where
  someSpace = KOSCParser $ buildSomeSpaceParser someSpace scalaCommentStyle

-- | Style for type/term variable and top-level binding identifiers.
varStyle :: CharParsing m => IdentifierStyle m
varStyle = IdentifierStyle
            { _styleName      = "identifier"
            , _styleStart     = letter
            , _styleLetter    = alphaNum <|> oneOf "_"
            , _styleReserved = HS.fromList
                [ "return", "record", "module", "import", "as"
                , "unqualified", "builtin", "structure", "get", "set"
                , "until", "if", "else", "for", "cast" ]
            , _styleHighlight = H.Identifier
            , _styleReservedHighlight = H.ReservedIdentifier
            }

opStyle :: TokenParsing m => IdentifierStyle m
opStyle = emptyOps
  { _styleReserved = HS.fromList ["+", "-", "*", "/"]
  }

opSym :: Op -> String
opSym o = case o of
  OpPlus  -> "+"
  OpMinus -> "-"
  OpMult  -> "*"
  OpDiv   -> "/"

-- | Parses a variable identifier.
rawNameP :: KOSCParser RawName
rawNameP = RawName <$> sepBy1 (ident varStyle) (symbol "::")

-- | Parses a reserved identifier.
reserved ::  String -> KOSCParser ()
reserved = reserve varStyle

-- | Parses a reserved operator.
reservedOp ::  String -> KOSCParser ()
reservedOp = reserve opStyle

opTable :: [[Operator KOSCParser (Expr RawName)]]
opTable = [ map (entry AssocLeft) [OpMult, OpDiv]
          , map (entry AssocLeft) [OpPlus, OpMinus]
          ]
  where
    entry a o = Infix (binOp o <$ reservedOp (opSym o)) a
    binOp o x y = EOp x o y

exprP :: KOSCParser (Expr RawName)
exprP = buildExpressionParser opTable accessorChainP

accessorChainP ::  KOSCParser (Expr RawName)
accessorChainP = do
  inner <- argP
  let chain inner = do
        cur <- optional $ (EAccessor inner Nothing <$> (dot *> ident varStyle))
               <|> (EIndex inner Nothing <$> brackets exprP)
               <|> (ECall inner <$> option [] (angles (commaSep typeP)) <*> parens (commaSep exprP))
        case cur of
          Just next -> chain next
          Nothing   -> return inner
  chain inner

argP :: KOSCParser (Expr RawName)
argP = choice [stringP, scalarP, unknownP, try recordInitP, castP, varP, parens exprP]

stringP :: KOSCParser (Expr RawName)
stringP = EString <$> stringLiteral

scalarP :: KOSCParser (Expr RawName)
scalarP = EScalar . either fromIntegral id <$> integerOrDouble

varP :: KOSCParser (Expr RawName)
varP = EVar <$> rawNameP

castP :: KOSCParser (Expr RawName)
castP = ECast <$> (reserved "cast" *> angles typeP) <*> parens exprP

unknownP :: KOSCParser (Expr RawName)
unknownP = EUnknown <$ symbol "?"

recordInitP :: KOSCParser (Expr RawName)
recordInitP = ERecordInit <$> rawNameP <*> option [] (angles (commaSep typeP)) <*> braces (commaSep fieldInitP) where
  fieldInitP = (,) <$> ident varStyle <* symbol "=" <*> exprP

testExpr :: String -> (Expr RawName)
testExpr str = case parseString (runKOSCParser $ exprP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e   -> e

-- * Statement Parser

stmtP :: KOSCParser (Stmt RawName)
stmtP = choice [stmtIfP, stmtUntilP, stmtForEachP] <|>
  (choice [stmtReturnP, try stmtDeclP, try stmtAssignP, stmtExprP] <* semi) <|> stmtBlockP

stmtDeclP :: KOSCParser (Stmt RawName)
stmtDeclP = SDeclVar <$> typeP <*> ident varStyle <* symbol "=" <*> exprP

stmtAssignP :: KOSCParser (Stmt RawName)
stmtAssignP = SAssign <$> exprP <* symbol "=" <*> exprP

stmtExprP :: KOSCParser (Stmt RawName)
stmtExprP = SExpr <$> exprP

stmtReturnP :: KOSCParser (Stmt RawName)
stmtReturnP = SReturn <$> (reserved "return" *> exprP)

stmtBlockP :: KOSCParser (Stmt RawName)
stmtBlockP = SBlock <$> stmtsP

stmtsP :: KOSCParser [Stmt RawName]
stmtsP = braces (many stmtP)

stmtUntilP :: KOSCParser (Stmt RawName)
stmtUntilP = SUntil <$> (reserved "until" *> parens exprP) <*> stmtsP

stmtIfP :: KOSCParser (Stmt RawName)
stmtIfP = SIf <$> (reserved "if" *> parens exprP) <*> stmtsP <*> option [] (reserved "else" *> stmtsP)

stmtForEachP :: KOSCParser (Stmt RawName)
stmtForEachP = SForEach <$> (reserved "for" *> symbol "(" *> typeP) <*> ident varStyle
               <* symbol ":" <*> exprP <* symbol ")" <*> stmtsP

testStmt :: String -> (Stmt RawName)
testStmt str = case parseString (runKOSCParser $ stmtP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e   -> e

-- * Declaration Parser

moduleNameP :: KOSCParser ModuleName
moduleNameP = ModuleName <$> sepBy1 (ident varStyle) (symbol "::")

moduleP :: KOSCParser (Module RawName)
moduleP = Module <$> (reserved "module" *> moduleNameP <* symbol ";") <*> many declP

declP :: KOSCParser (Decl RawName)
declP = choice [DeclImport <$> importDeclP, DeclRec <$> try recDeclP, DeclVar <$> try varDeclP, DeclFun <$> funDeclP, DeclBuiltin <$> builtinP]

importDeclP :: KOSCParser ImportDecl
importDeclP = ImportDecl <$> (reserved "import" *> moduleNameP)
  <*> optional (reserved "as" *> moduleNameP)
  <*> option False (True <$ reserved "unqualified")
  <* semi

visibilityP :: KOSCParser Visibility
visibilityP = choice [Public <$ reserved "public", Private <$ reserved "private" ]

funSigP :: KOSCParser (FunSig RawName)
funSigP = FunSig <$> option Public visibilityP
           <*> typeP  -- return type
           <*> ident varStyle -- name
           <*> option [] (angles $ commaSep $ ident varStyle) -- generic parameters
           <*> parens (commaSep paramP) -- parameters

varSigP :: KOSCParser (VarSig RawName)
varSigP = VarSig <$> option Public visibilityP
           <*> typeP
           <*> ident varStyle
           <*> accessibilityP


indexSigP :: KOSCParser (IndexSig RawName)
indexSigP = IndexSig <$> option Public visibilityP
           <*> typeP
           <* symbol "[" <*> typeP <*> ident varStyle <* symbol "]"
           <*> accessibilityP

fieldSigP :: KOSCParser (FieldSig RawName)
fieldSigP = (FieldFunSig <$> try funSigP) <|> (FieldVarSig <$> try varSigP) <|> (FieldIndexSig <$> indexSigP)

structSigP :: KOSCParser (StructSig RawName)
structSigP = StructSig <$> option Public visibilityP
  <*> (reserved "structure" *> ident varStyle)
  <*> option [] (angles $ commaSep1 $ ident varStyle)
  <*> optional (colon *> typeP)
  <*> braces (many (fieldSigP <* semi))

funDeclP :: KOSCParser (FunDecl RawName)
funDeclP = FunDecl <$> funSigP <*> braces (many stmtP)

varDeclP :: KOSCParser (VarDecl RawName)
varDeclP = VarDecl <$> varSigP <* symbol "=" <*> exprP <* semi

recDeclP :: KOSCParser (RecDecl RawName)
recDeclP = RecDecl <$> option Public visibilityP
  <*> (reserved "record" *> ident varStyle)
  <*> option [] (angles $ commaSep1 $ ident varStyle)
  <*> braces (many (varSigP <* semi))

paramP :: KOSCParser (Param RawName)
paramP = Param <$> typeP <*> ident varStyle <*> optional (symbol "=" *> exprP)

typeP :: KOSCParser (Type RawName)
typeP = do
  retTy <- TypeGeneric <$> rawNameP <*> option [] (angles $ commaSep typeP)
  maybeFun <- optional ((,) <$> parens (commaSep typeP) <*> option [] (brackets (commaSep typeP)))
  case maybeFun of
    Nothing -> return retTy
    Just (args, opts) -> return (TypeFunction retTy args opts)

accessibilityP :: KOSCParser Accessibility
accessibilityP = option GetOrSet $ (Get <$ reserved "get") <|> (Set <$ reserved "set")

builtinP :: KOSCParser (Builtin RawName)
builtinP = reserved "builtin" *> choice [BuiltinStruct <$> try structSigP, BuiltinFun <$> try funSigP <* semi, BuiltinVar <$> varSigP <* semi]

testModule :: String -> (Module RawName)
testModule str = case parseString (runKOSCParser $ moduleP <* eof) mempty str of
  Failure doc -> error $ show doc
  Success e   -> e
