{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module KOSC.Compiler.CodeGen where

import           Control.Lens
import           Control.Monad.State
import           Data.List
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import qualified Data.Text.Lazy                as L
import           Numeric                       (showGFloat, showIntAtBase)
import           Text.InterpolatedString.Perl6 (qq)
import qualified Text.PrettyPrint.ANSI.Leijen  as PP

import           KOSC.Compiler.Common
import           KOSC.Compiler.ScopeChecker
import qualified KOSC.Language.AST             as AST

-- | The type of generated names. Mainly used for identifying what a function actually returns instead of just text.
type GeneratedName = L.Text

-- | Type of generated code.
type GeneratedCode = L.Text

type GlobalName = [AST.Ident]

-- | Possible declarations for things that are types
data TypeDecl
  = Record (AST.RecDecl AST.ScopedName) -- ^ the type is a record
  | BuiltinStruct (AST.StructSig AST.ScopedName) -- ^ the type is a builtin structure

-- | Possible declarations for things that are variables.
data TermDecl
  = Fun (AST.FunDecl AST.ScopedName) -- ^ refers to a function
  | Var (AST.VarDecl AST.ScopedName) -- ^ refers to variable
  | BuiltinFun (AST.FunSig AST.ScopedName) -- ^ refers to a builtin function
  | BuiltinVar (AST.VarSig AST.ScopedName) -- ^ refers to a builtin variable

-- | Internal state of the code generator.
data CodeGenState = CodeGenState
  { _generatedNames   :: Map GlobalName GeneratedName
  -- ^ Mapping from global names (source) to generated names (target). This is also used for making sure every declaration is just created once.
  , _generatedLocalNames :: Map AST.Ident GeneratedName
  -- ^ Mapping from local names to generated names.
  , _typeDeclarations :: Map AST.ScopedName TypeDecl
  -- ^ Type declarations of all imported modules. When dealing with records, we need to inspect the type of the things that are accessed.
  , _termDeclarations :: Map AST.ScopedName TermDecl
  -- ^ Term level declarations (i.e. global functions and variables) of all imported modules.
  , _generatedCode    :: Map GlobalName L.Text
  -- ^ Mapping from top-level entities (functions or vars so far) to the code that has been generated for them (if any).
  , _priorities       :: Map GlobalName Integer
  -- ^ Mapping from top-level entities (functions or vars so far) to a priority indicating their position in the generated file.
  , _fresh            :: Integer
  -- ^ Used for generating fresh names.
  , _priority         :: Integer
  -- ^ Used for generating priority numbers
  }

makeLenses ''CodeGenState

-- | The code generation monad. It carries the state on top of the underlying compiler monad.
type CodeGenM m = StateT CodeGenState (KOSCCompilerT m)

-- | Entry point of the code generator.
codeGen :: Monad m
        => Map AST.ModuleName ScopedModule -- ^ all transitive includes of the main module (should also contain the main module itself)
        -> AST.ModuleName                  -- ^ the main module (containing a function "Main")
        -> KOSCCompilerT m L.Text
codeGen imports mainModule = evalStateT go initialState where
  -- helper functions for generating maps from fully qualified names to things defined in a module
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
    , _generatedLocalNames = Map.empty
    -- throw together all declarations from all imported modules
    , _typeDeclarations = foldOf (folded . scopedModuleAST . to (exportDecls exportTypeDecl) ) imports
    , _termDeclarations = foldOf (folded . scopedModuleAST . to (exportDecls exportTermDecl) ) imports
    , _generatedCode = Map.empty
    , _priorities = Map.empty
    , _fresh = 0
    , _priority = 0
    }

  go = do
    let mainName = AST.makeGlobalName mainModule "Main"
    hasMain <- uses termDeclarations (Map.member mainName)
    unless hasMain $ criticalWithContext MessageNoMain
    -- start code generation on main module
    generatedMainName <- generateName mainName
    code <- use generatedCode
    -- order generated code by priorities
    prs <- use priorities
    let orderedCode = Map.mapKeys (\name -> Map.findWithDefault 0 name prs) code
        declCode = foldMap snd $ Map.toDescList orderedCode
    -- emit a call to the generated main function at the end of the script
    return $ L.append declCode [qq|$generatedMainName().|]

-- | Generates a fresh name. Currently, it uses base 36 numbers prefixed with an underscore
-- in order to make names as short as possible.
freshName :: Monad m => CodeGenM m GeneratedName
freshName = nameBase36 <$> (fresh <<+= 1)

-- | Generates a fresh name. Currently, it uses base 36 numbers prefixed with an underscore
-- in order to make names as short as possible.
nextPriority :: Monad m => CodeGenM m Integer
nextPriority = priority <<+= 1

-- | Generates the entity referred to by the scoped name, if it has not yet been generated.
-- Returns the generated name of the requested entity.
-- This is the core of the lazy code generation. If a name is never requested, it will never generate code for its definition.
generateIfRequired :: Monad m => [AST.Ident] -> CodeGenM m GeneratedName
generateIfRequired sname = use (generatedNames . at sname) >>= \case
  Just genName -> return genName -- we already generated that
  Nothing -> globalScope $ do
    -- assign next priority number for this thing
    nextP <- nextPriority
    priorities . at sname .= Just nextP
    use (termDeclarations . at (AST.ScopedGlobal sname)) >>= \case
      Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Undeclared identifier found in code generator. This is a bug."
      Just td -> case td of
        Fun fdef -> do
          -- it is important to FIRST generate and register the name, and then start generating the function body
          -- otherwise, this would fail for recursive functions
          name <- maybe freshName (pure . L.pack) (fdef ^. AST.funDeclSignature . AST.funSigOutputName)
          generatedNames . at sname .= Just name
          fcode <- enterDecl (fdef ^. AST.funDeclSignature . AST.funSigName) $ generateFunction name fdef
          generatedCode . at sname .= Just fcode
          return name
        Var vdef -> do
          -- variables are handled similar to functions
          name <- freshName
          generatedNames . at sname .= Just name
          vcode <- enterDecl (vdef ^. AST.varDeclSignature . AST.varSigName) $ generateVar name vdef
          generatedCode . at sname .= Just vcode
          return name
          -- builtins do not generate code, and we never rename them
        BuiltinFun sig -> do
          let genname = L.pack $ fromMaybe (sig ^. AST.funSigName) (sig ^. AST.funSigOutputName)-- preserve names for builtins
          generatedNames . at sname .= Just genname
          return genname
        BuiltinVar sig -> do
          let genname = L.pack $ fromMaybe (sig ^. AST.varSigName) (sig ^. AST.varSigOutputName)-- preserve names for builtins
          generatedNames . at sname .= Just genname
          return genname

-- | Generates code for a name. Local variables are returned as-is, global variables might trigger further code generation.
generateName :: Monad m => AST.ScopedName -> CodeGenM m GeneratedName
generateName (AST.ScopedLocal l) = uses generatedLocalNames (Map.findWithDefault (L.pack l) l)
generateName (AST.ScopedGlobal g) = generateIfRequired g
generateName (AST.ScopedAmbiguous _) = criticalWithContext $ MessageUnspecified "Encountered ambiguous name in code generator. This is a bug."

-- | Assigns a new generated name to a local variable
newLocalName :: Monad m => AST.Ident -> CodeGenM m GeneratedName
newLocalName var = do
  newName <- freshName
  generatedLocalNames . at var .= Just newName
  return newName

-- | Generates code for an expression.
generateExpression :: Monad m => AST.Expr AST.ScopedName -> CodeGenM m GeneratedCode
generateExpression e = go 0 e where
  -- helper function to emit parenthesis when required by operator precedence
  -- All generation function take an argument "outerPrec" that denotes how strong the enclosing operator binds.
  -- If the enclosing operator binds stronger than the current operation being generated, parentheses are added.
  precParens requiredPrec actualPrec str
    | actualPrec < requiredPrec = [qq|($str)|]
    | otherwise = str

  -- info table for binary operators, it returns a quadruple consisting
  -- of the operator symbol, its precedence, and the required precedence of the left and right arguments.
  -- The latter two are used to implement associativity rules.
  binOpInfo :: AST.BinOp -> (L.Text, Int, Int, Int)
  binOpInfo op = case op of
        AST.BinOpPlus    -> ("+", 6, 6, 7)
        AST.BinOpMinus   -> ("-", 6, 6, 7)
        AST.BinOpMult    -> ("*", 7, 7, 8)
        AST.BinOpDiv     -> ("/", 7, 7, 8)
        AST.BinOpPow     -> ("^", 9, 9, 10)
        AST.BinOpAnd     -> ("and", 3, 4, 3)
        AST.BinOpOr      -> ("or", 2, 3, 2)
        AST.BinOpEq      -> ("=", 4, 5, 5)
        AST.BinOpNeq     -> ("<>", 4, 5, 5)
        AST.BinOpLeq     -> ("<=", 4, 5, 5)
        AST.BinOpGeq     -> (">=", 4, 5, 5)
        AST.BinOpLess    -> ("<", 4, 5, 5)
        AST.BinOpGreater -> (">", 4, 5, 5)

  -- info table for unary operators, consisting of the symbol and precedence
  unOpInfo :: AST.UnOp -> (L.Text, Int)
  unOpInfo op = case op of
    AST.UnOpNegate -> ("-", 8)
    AST.UnOpNot    -> ("not", 8)

  --------- GENERATION FUNCTIONS

  -- generates an accessor for a structure
  goStructAccessor outerPrec accessed field = do
    acode <- go 10 accessed
    return $ precParens outerPrec 10 [qq|$acode:$field|]

  -- generates an accessor for a record field.
  -- It looks up the index of the field in the record declaration and generates the corresponding list indexing expression.
  goRecordAccessor outerPrec accessed record field = case elemIndex field $ toListOf (AST.recDeclVars . folded . AST.varSigName) record of
    Nothing
      -- hard code generation of "Copy" field
      | field == "Copy" -> do
        acode <- go 10 accessed
        return $ precParens outerPrec 10 [qq|$acode:$field|]
      | otherwise -> criticalWithContext $ MessageUnspecified $ PP.text "Encountered unknown record field in code generator. This is a bug."
    Just idx -> do
      acode <- go 10 accessed
      return $ precParens outerPrec 10 [qq|$acode[$idx]|]

  -- actual recursion for generating expressions. Most cases are wrapped in a use of "precParens" to insert parentheses when needed
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
      let (sym, selfPrec, lprec, rprec) = binOpInfo o
      lcode <- go lprec e1
      rcode <- go rprec e2
      return $ precParens outerPrec selfPrec [qq|$lcode $sym $rcode|]
    AST.EUnOp op e -> do
      let (sym, selfPrec) = unOpInfo op
      code <- go selfPrec e
      return $ precParens outerPrec selfPrec [qq|$sym $code|]
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
            missingInitializers = fieldNames \\ map (view _1) fields
            dummyVal = AST.EScalar 0 -- dummy value for uninitialized fields
            initArgs = map (\v -> fromMaybe dummyVal $ lookup v fields ) fieldNames
        when (not $ null missingInitializers) $
          messageWithContext MessageWarning $ MessageUninitializedIdentifiers missingInitializers
        argCode <- mapM (go 0) initArgs
        -- compile as list initializer
        return $ precParens outerPrec 10 [qq|list({L.intercalate "," argCode})|]
    AST.EUnknown -> criticalWithContext $ MessageUnspecified $ PP.text "Encountered unknown expression in code generator. This is a bug."
    AST.ECast _ e -> go outerPrec e
    AST.ELambda params _ body -> generateFunctionBody params body
    AST.EAt e -> do
      ecode <- go outerPrec e
      return [qq|$ecode@|]

-- | Generates a list of statements.
generateStatements :: Monad m => [AST.Stmt AST.ScopedName] -> CodeGenM m GeneratedCode
generateStatements stmts = L.concat <$> traverse generateStatement stmts

-- | Generates code for a single statement.
generateStatement :: Monad m => AST.Stmt AST.ScopedName -> CodeGenM m GeneratedCode
generateStatement s = case s of
  AST.SDeclVar _ name init -> do
    genname <- newLocalName name
    icode <- generateExpression init
    return $ [qq|LOCAL $genname IS $icode.$ln|]
  AST.SAssign lhs rhs -> do
    lcode <- generateExpression lhs
    rcode <- generateExpression rhs
    return $ [qq|SET $lcode TO $rcode.$ln|]
  AST.SReturn ret -> do
    rcode <- maybe (pure "") generateExpression ret
    return $ [qq|RETURN $rcode.$ln|]
  AST.SExpr e -> do
    code <- generateExpression e
    return $ [qq|$code.$ln|]
  AST.SBlock stmts -> do
    code <- nestedScope $ generateStatements stmts
    return [qq|\{$code\}$ln|]
  AST.SIf cond sthen selse -> do
    ccode <- generateExpression cond
    tcode <- nestedScope $ generateStatements sthen
    ecode <- nestedScope $ generateStatements selse
    let elsePart = if null selse then ln else [qq| else \{$ln$ecode\}$ln|]
    return $ [qq|if $ccode \{$ln $tcode \}$elsePart|]
  AST.SUntil cond body -> do
    bcode <- nestedScope $ generateStatements body
    ccode <- generateExpression cond
    return [qq|until $ccode \{$ln$bcode\}$ln|]
  AST.SForEach _ name expr body -> do
    genname <- newLocalName name
    ecode <- generateExpression expr
    bcode <- nestedScope $ generateStatements body
    return [qq|for $genname in $ecode \{$ln$bcode\}$ln|]
  AST.SBreak -> return "break.\n"
  AST.SLock var expr -> do
    name <- generateName var
    ecode <- generateExpression expr
    return [qq|lock $name to $ecode.$ln|]
  AST.SUnlock Nothing -> return "unlock all.\n"
  AST.SUnlock (Just var) -> do
    name <- generateName var
    return [qq|unlock $name.$ln|]
  AST.SWait dur -> do
    dcode <- generateExpression dur
    return [qq|wait $dcode.$ln|]
  AST.SWaitUntil cond -> do
    ccode <- generateExpression cond
    return [qq|wait until $ccode.$ln|]
  AST.SOn change body -> do
    ccode <- generateExpression change
    bcode <- nestedScope $ generateStatements body
    return [qq|on $ccode \{$ln$bcode\}$ln|]
  AST.SWhen cond body -> do
    ccode <- generateExpression cond
    bcode <- nestedScope $ generateStatements body
    return [qq|when $ccode then \{$ln$bcode\}$ln|]
  AST.SRaw (AST.RawCode parts) -> do
    parts <- forM parts $ \case
      AST.RawCodeText txt -> pure (L.fromStrict txt)
      AST.RawCodeName name -> generateName name
    return $ L.concat parts

-- | Generates code for a function declaration.
generateFunction :: Monad m => GeneratedName -> AST.FunDecl AST.ScopedName -> CodeGenM m L.Text
generateFunction actualName decl = do
  body <- generateFunctionBody (decl ^. AST.funDeclSignature . AST.funSigParameters) (decl ^. AST.funDeclStatements)
  let code = [qq|FUNCTION $actualName $body$ln|]
  return code

-- | Generates for a function body. This is used for both lambda functions and actual declarations.
generateFunctionBody :: Monad m => [AST.Param AST.ScopedName] -> [AST.Stmt AST.ScopedName] -> CodeGenM m L.Text
generateFunctionBody params stmts = do
  let param (AST.Param _ name Nothing) = newLocalName name
      param (AST.Param _ name (Just e)) = do
        genname <- newLocalName name
        ecode <- generateExpression e
        return [qq|$genname IS $ecode|]
  pcode <- mapM param params
  body <- nestedScope $ generateStatements stmts
  let paramDecl = if null pcode then L.empty else [qq|PARAMETER {L.intercalate "," pcode}.$ln|]
      code = [qq|\{$ln$paramDecl$body\}$ln|]
  return code

-- | Generates a global variable.
generateVar :: Monad m => GeneratedName -> AST.VarDecl AST.ScopedName -> CodeGenM m L.Text
generateVar actualName decl = do
  icode <- nestedScope $ generateExpression (decl ^. AST.varDeclInitializer)
  return [qq|DECLARE GLOBAL $actualName TO $icode.$ln|]

-- | Temporarily switches into global scope for processing the nested action.
-- This resets all fields of the state holding information that is specific to a
-- local scope. The local state is restored after the nested operation has
-- finished.
globalScope :: Monad m => CodeGenM m a -> CodeGenM m a
globalScope nested = nestedScope $ do
  generatedLocalNames .= Map.empty
  nested

-- | Creates a new scope nesting level. All modifications to the local scope
-- performed by the nested action are undone after it has finished.
nestedScope :: Monad m => CodeGenM m a -> CodeGenM m a
nestedScope nested = do
  locNames <- use generatedLocalNames
  r <- nested
  generatedLocalNames .= locNames
  return r

-- | Converts a number to a valid name by displaying it in base 36 and prefixing it with an underscore.
nameBase36 :: Integer -> GeneratedName
nameBase36 = L.pack . ('_' :) . showBase36 where
  chars = ['0'..'9'] ++ ['a'..'z']
  toChar i = chars !! i
  showBase36 i = showIntAtBase 36 toChar i ""

-- | Newline character that is used in the string interpolations.
ln :: L.Text
ln = "\n"
