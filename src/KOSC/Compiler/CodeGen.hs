{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module KOSC.Compiler.CodeGen where

import           Control.Lens
import           Control.Monad.State
import           Data.Foldable
import           Data.List
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe
import qualified Data.Text.Lazy                 as L
import           Numeric                        (showIntAtBase, showGFloat)
import           Text.InterpolatedString.Perl6  (qq)
import qualified Text.PrettyPrint.ANSI.Leijen   as PP

import           KOSC.Compiler.Common
import           KOSC.Compiler.ScopeChecker
import qualified KOSC.Language.AST              as AST

type GeneratedName = L.Text

type GeneratedCode = L.Text

-- | Possible declarations for things that are types
data TypeDecl = Record (AST.RecDecl AST.ScopedName) | BuiltinStruct (AST.StructSig AST.ScopedName)

-- | Possible declarations for things that are variables.
data TermDecl = Fun (AST.FunDecl AST.ScopedName) | Var (AST.VarDecl AST.ScopedName)
  | BuiltinFun (AST.FunSig AST.ScopedName) | BuiltinVar (AST.VarSig AST.ScopedName)

data CodeGenState = CodeGenState
  { _generatedNames   :: Map AST.ScopedName GeneratedName
  , _typeDeclarations :: Map AST.ScopedName TypeDecl
  , _termDeclarations :: Map AST.ScopedName TermDecl
  , _generatedCode    :: Map AST.ScopedName L.Text
  , _fresh            :: Integer
  }

makeLenses ''CodeGenState

type CodeGenM m = StateT CodeGenState (KOSCCompilerT m)

codeGen :: Monad m => Map AST.ModuleName ScopedModule -> AST.ModuleName -> KOSCCompilerT m L.Text
codeGen imports mainModule = evalStateT go initialState where
  exportDecls which mod = let modname = mod ^. AST.moduleName
                          in Map.fromList [ (AST.makeGlobalName modname name, d) | decl <- view AST.declarations mod, (name, d) <- which decl ]

  exportTermDecl (AST.DeclBuiltin (AST.BuiltinFun sig)) = [(sig ^. AST.funSigName, BuiltinFun sig)]
  exportTermDecl (AST.DeclBuiltin (AST.BuiltinVar sig)) = [(sig ^. AST.varSigName, BuiltinVar sig)]
  exportTermDecl (AST.DeclFun d@(AST.FunDecl sig _)) = [(sig ^. AST.funSigName, Fun d)]
  exportTermDecl (AST.DeclVar d@(AST.VarDecl sig _)) = [(sig ^. AST.varSigName, Var d)]
  exportTermDecl _ = []

  exportTypeDecl (AST.DeclBuiltin (AST.BuiltinStruct sig)) = [(sig ^. AST.structSigName, BuiltinStruct sig)]
  exportTypeDecl (AST.DeclRec d@(AST.RecDecl _ name _ _)) = [(name, Record d)]
  exportTypeDecl _ = []

  initialState = CodeGenState
    { _generatedNames = Map.empty
    , _typeDeclarations = foldOf (folded . scopedModuleAST . to (exportDecls exportTypeDecl) ) imports
    , _termDeclarations = foldOf (folded . scopedModuleAST . to (exportDecls exportTermDecl) ) imports
    , _generatedCode = Map.empty
    , _fresh = 0
    }

  go = do
    mainName <- generateIfRequired (AST.makeGlobalName mainModule "Main")
    declCode <- uses generatedCode fold
    return $ L.append declCode [qq|$mainName().|]

freshName :: Monad m => CodeGenM m GeneratedName
freshName = nameBase36 <$> (fresh <<+= 1)

-- | Emits a piece of raw code.
emitDeclCode :: Monad m => AST.ScopedName -> L.Text -> CodeGenM m ()
emitDeclCode name code = generatedCode . at name .= Just code

-- | Generates the entity referred to by the scoped name, if it has not yet been generated.
-- Returns the generated name of the requested entity.
generateIfRequired :: Monad m => AST.ScopedName -> CodeGenM m GeneratedName
generateIfRequired sname = use (generatedNames . at sname) >>= \case
  Just genName -> return genName
  Nothing -> use (termDeclarations . at sname) >>= \case
    Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Undeclared identifier found in code generator. This is a bug."
    Just td -> case td of
      Fun fdef -> do
        -- it is important to FIRST generate and register the name, and then start generating the function body
        -- otherwise, this would fail for recursive functions
        name <- freshName
        generatedNames . at sname .= Just name
        fcode <- generateFunction name fdef
        generatedCode . at sname .= Just fcode
        return name
      Var vdef -> do
        name <- freshName
        generatedNames . at sname .= Just name
        vcode <- generateVar name vdef
        generatedCode . at sname .= Just vcode
        return name
      BuiltinFun sig -> do
        let genname = L.pack $ sig ^. AST.funSigName -- preserve names for builtins
        generatedNames . at sname .= Just genname
        return genname
      BuiltinVar sig -> do
        let genname = L.pack $ sig ^. AST.varSigName -- preserve names for builtins
        generatedNames . at sname .= Just genname
        return genname

generateName :: Monad m => AST.ScopedName -> CodeGenM m GeneratedName
generateName (AST.ScopedLocal l) = return $ L.pack l
generateName g@(AST.ScopedGlobal _) = generateIfRequired g
generateName (AST.ScopedAmbiguous _) = criticalWithContext $ MessageUnspecified "Encountered ambiguous name in code generator. This is a bug."

generateExpression :: Monad m => AST.Expr AST.ScopedName -> CodeGenM m L.Text
generateExpression e = go 0 e where
  precParens requiredPrec actualPrec str
    | actualPrec < requiredPrec = [qq|($str)|]
    | otherwise = str

  genOp :: AST.BinOp -> (L.Text, Int, Int, Int)
  genOp op = case op of
        AST.BinOpPlus -> ("+", 6, 6, 7)
        AST.BinOpMinus -> ("-", 6, 6, 7)
        AST.BinOpMult -> ("*", 7, 7, 8)
        AST.BinOpDiv -> ("/", 7, 7, 8)
        AST.BinOpPow -> ("^", 8, 8, 9)
        AST.BinOpAnd -> ("and", 3, 4, 3)
        AST.BinOpOr -> ("or", 2, 3, 2)
        AST.BinOpEq -> ("=", 4, 5, 5)
        AST.BinOpNeq -> ("<>", 4, 5, 5)
        AST.BinOpLeq -> ("<=", 4, 5, 5)
        AST.BinOpGeq -> (">=", 4, 5, 5)
        AST.BinOpLess -> ("<", 4, 5, 5)
        AST.BinOpGreater -> (">", 4, 5, 5)

  goStructAccessor outerPrec accessed field = do
    acode <- go 10 accessed
    return $ precParens outerPrec 10 [qq|$acode:$field|]

  goRecordAccessor outerPrec accessed record field = case elemIndex field $ toListOf (AST.recDeclVars . folded . AST.varSigName) record of
    Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Encountered unknown record field in code generator. This is a bug."
    Just idx -> do
      acode <- go 10 accessed
      return $ precParens outerPrec 10 [qq|$acode[$idx]|]

  go outerPrec e = case e of
    AST.EVar name -> generateName name
    AST.EAccessor accessed whatTy field -> case whatTy of
      Just (AST.TypeGeneric name _) -> use (typeDeclarations . at name) >>= \case
        Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Encountered unknown record in code generator. This is a bug."
        Just (BuiltinStruct _) -> goStructAccessor outerPrec accessed field
        Just (Record r) -> goRecordAccessor outerPrec accessed r field
      Just (AST.TypeFunction _ _ _) -> goStructAccessor outerPrec accessed field
      Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Type checker did not provide enough information. This is a bug."
    AST.EIndex indexed _ arg -> do
      icode <- go 10 indexed
      acode <- go 0 arg
      return $ precParens outerPrec 10 [qq|$icode[$acode]|]
    AST.EOp e1 o e2 -> do
      let (sym, selfPrec, lprec, rprec) = genOp o
      lcode <- go lprec e1
      rcode <- go rprec e2
      return $ precParens outerPrec selfPrec [qq|$lcode $sym $rcode|]
    AST.ECall callee _ args -> do
      ccode <- go 10 callee
      argCode <- mapM (go 0) args
      return $ precParens outerPrec 10 [qq|$ccode({L.intercalate "," argCode})|]
    AST.EScalar val -> return $ L.pack $ showGFloat Nothing val ""
    AST.EString str -> return $ L.pack $ show str
    AST.ERecordInit name _ fields -> use (typeDeclarations . at name) >>= \case
      Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Encountered unknown record in code generator. This is a bug."
      Just (BuiltinStruct _) -> criticalWithContext $ MessageUnspecified $ PP.text "Cannot use record initializer on builtin structures. (FIXME: Move this check to type checker)"
      Just (Record r) -> do
        let fieldNames = toListOf (AST.recDeclVars . traversed . AST.varSigName) r
            missingInitializers = map (view _1) fields
            dummyVal = AST.EScalar 0 -- dummy value for uninitialized fields
            initArgs = map (\v -> fromMaybe dummyVal $ lookup v fields ) fieldNames
        messageWithContext MessageWarning $ MessageUninitializedIdentifiers missingInitializers
        argCode <- mapM (go 0) initArgs
        -- compile as list initializer
        return $ precParens outerPrec 10 [qq|list({L.intercalate "," argCode})|]
    AST.EUnknown -> criticalWithContext $ MessageUnspecified $ PP.text "Encountered unknown expression in code generator. This is a bug."
    AST.ECast _ e -> go outerPrec e

generateStatements :: Monad m => [AST.Stmt AST.ScopedName] -> CodeGenM m L.Text
generateStatements stmts = L.concat <$> traverse generateStatement stmts

generateStatement :: Monad m => AST.Stmt AST.ScopedName -> CodeGenM m L.Text
generateStatement s = case s of
  AST.SDeclVar _ name init -> do
    icode <- generateExpression init
    return $ [qq|LOCAL $name IS $icode.$ln|]
  AST.SAssign lhs rhs -> do
    lcode <- generateExpression lhs
    rcode <- generateExpression rhs
    return $ [qq|SET $lcode TO $rcode.$ln|]
  AST.SReturn ret -> do
    rcode <- generateExpression ret
    return $ [qq|RETURN $rcode.$ln|]
  AST.SExpr e -> do
    code <- generateExpression e
    return $ [qq|$code.$ln|]
  AST.SBlock stmts -> do
    code <- generateStatements stmts
    return [qq|\{$code\}$ln|]
  AST.SIf cond sthen selse -> do
    tcode <- generateStatements sthen
    ecode <- generateStatements selse
    ccode <- generateExpression cond
    let elsePart = if null selse then ln else [qq| else \{$ln$ecode\}$ln|]
    return $ [qq|if $ccode \{$ln $tcode \}$elsePart|]
  AST.SUntil cond body -> do
    bcode <- generateStatements body
    ccode <- generateExpression cond
    return [qq|until $ccode \{$ln$bcode\}$ln|]
  AST.SForEach _ name expr body -> do
    ecode <- generateExpression expr
    bcode <- generateStatements body
    return [qq|for $name in $ecode \{$ln$bcode\}$ln|]

generateFunction :: Monad m => GeneratedName -> AST.FunDecl AST.ScopedName -> CodeGenM m L.Text
generateFunction actualName decl = do
  let param (AST.Param _ name Nothing) = return $ L.pack name
      param (AST.Param _ name (Just e)) = do
        ecode <- generateExpression e
        return [qq|$name IS $ecode|]
  pcode <- mapM param (decl ^. AST.funDeclSignature . AST.funSigParameters)
  body <- generateStatements (decl ^. AST.funDeclStatements)
  let paramDecl = if null pcode then L.empty else [qq|PARAMETER {L.intercalate "," pcode}.$ln|]
      code = [qq|FUNCTION $actualName \{$ln$paramDecl$body\}$ln|]
  return code

generateVar :: Monad m => GeneratedName -> AST.VarDecl AST.ScopedName -> CodeGenM m L.Text
generateVar actualName decl = do
  icode <- generateExpression (decl ^. AST.varDeclInitializer)
  return [qq|DECLARE GLOBAL $actualName TO $icode.$ln|]

nameBase36 :: Integer -> GeneratedName
nameBase36 = L.pack . ('_' :) . showBase36 where
  chars = ['0'..'9'] ++ ['a'..'z']
  toChar i = chars !! i
  showBase36 i = showIntAtBase 36 toChar i ""

ln :: L.Text
ln = "\n"
