{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-| This module implements type checking functionality. -}
module KOSC.Compiler.TypeChecker where

import           Control.Lens
import Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Foldable
import           Data.Semigroup
import           Data.List
import           Data.Maybe
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           System.Directory
import           System.FilePath
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Text.Trifecta                as Trifecta

import qualified KOSC.Language.AST                     as AST
import qualified KOSC.Language.Parser                  as Parser
import KOSC.Compiler.Common
import KOSC.Compiler.ImportResolution
import KOSC.Compiler.ScopeChecker

data TypeScheme = TypeScheme [AST.Ident] (AST.Type AST.ScopedName)

data TypeEnv = TypeEnv
  { _typeMap            :: Map AST.ScopedName TypeScheme
  , _requiredReturnType :: Maybe TypeScheme
  }

makeLenses ''TypeEnv

typeChecker :: Monad m => Map AST.ModuleName ScopedModule -> ScopedModule -> KOSCCompilerT m ()
typeChecker imports inputMod = evalStateT checkModule initialEnv where
  initialEnv = TypeEnv Map.empty Nothing
  
  checkModule = do
    populateGlobalTypeEnv imports
    enterModule (inputMod ^. scopedModuleAST . AST.moduleName) $
      mapMOf_ (scopedModuleAST . AST.declarations . folded) checkDecl inputMod

  checkDecl (AST.DeclImport _) = pure ()
  checkDecl (AST.DeclFun fdecl) = checkFunDecl fdecl
  checkDecl (AST.DeclBuiltin _) = pure ()

  checkFunDecl (AST.FunDecl sig body) = enterScope $ enterDecl (sig ^. AST.funSigName) $ do
    buildFunctionEnv sig
    mapM_ checkStmt body

  inferExpr (AST.EVar v) = use (typeMap . at v) >>= \case
    Nothing -> criticalWithContext $ MessageUnspecified $ PP.text "Undeclared variable found in type checker. This is a bug. Did the program pass the scope checker?"
    Just scheme -> return scheme
  inferExpr (AST.EAccessor e field) = undefined -- TODO: check accessors _
  inferExpr (AST.EIndex e eidx) = undefined -- TODO: check indexing expressions
  inferExpr (AST.EOp e1 op e2)
    | AST.isArithmeticOp op = do
        ty1 <- inferExpr e1
        ty2 <- inferExpr e2
        requireType scalarType ty1
        requireType scalarType ty2
        return scalarType
  inferExpr (AST.ECall f tyargs args) = do
    -- infer type of the called function
    TypeScheme generics ty <- inferExpr f
    -- number of generics must match
    when (length tyargs /= length generics) $ messageWithContext MessageError $ MessageGenericTypeMismatch
    let gensubst = Map.fromList $ zip generics tyargs
    case ty of
      AST.TypeFunction retTy manTys optTys -> do
        -- must have at least all mandatory arguments, and not more than the total number of arguments
        when (length args < length manTys) $
          messageWithContext MessageError $ MessageNotEnoughArguments
        when (length args > length manTys + length optTys) $
          messageWithContext MessageError $ MessageTooManyArguments
        -- match arguments individually
        argTys <- traverse inferExpr args
        zipWithM_ requireType (map (TypeScheme [] . substituteGenerics gensubst) $ manTys ++ optTys) argTys
        return $ TypeScheme [] $ substituteGenerics gensubst retTy
      other -> do
        messageWithContext MessageError $ MessageFunctionExpected other
        return $ TypeScheme [] $ AST.TypeName (AST.ScopedLocal "<UNKNOWN>")
  -- FIXME: make it so that these do not require a hard-coded type name
  inferExpr (AST.EScalar d) = pure scalarType
  inferExpr (AST.EString s) = pure stringType

  checkStmt (AST.SDeclVar ty name init) = do
    let declty = TypeScheme [] ty
    typeMap . at (AST.ScopedLocal name) .= Just declty
    tyInit <- inferExpr init
    requireType declty tyInit
  checkStmt (AST.SAssign lhs rhs) = do
    lhsty <- inferLHS lhs
    rhsty <- inferExpr rhs
    requireType lhsty rhsty
  checkStmt (AST.SReturn ret) = use requiredReturnType >>= \case
    Nothing -> messageWithContext MessageError $ MessageUnspecified $ PP.text "Cannot return from here"
    Just retType -> do
      actualType <- inferExpr ret
      requireType retType actualType
  checkStmt (AST.SExpr ex) = void $ inferExpr ex -- just make sure some type IS can be inferred
  checkStmt (AST.SBlock stmts) = enterScope $ mapM_ checkStmt stmts

  -- TODO: check that LHS is settable
  inferLHS e = inferExpr e

  -- predefined types
  scalarType = TypeScheme [] (AST.TypeName $ AST.ScopedGlobal ["Builtin", "Scalar"])
  stringType = TypeScheme [] (AST.TypeName $ AST.ScopedGlobal ["Builtin", "String"])

  -- saves current type environment and restores it on exit
  enterScope action = do
    env <- get
    r <- action
    put env
    return r

requireNonGeneric :: MonadCompiler MessageContent m => TypeScheme -> m (AST.Type AST.ScopedName)
requireNonGeneric (TypeScheme [] ty) = pure ty
requireNonGeneric (TypeScheme gen ty) = do
  messageWithContext MessageError $ MessageGenericTypeMismatch
  return ty

substituteGenerics :: Map AST.Ident (AST.Type AST.ScopedName) -> AST.Type AST.ScopedName -> AST.Type AST.ScopedName
substituteGenerics subst = go where
  go v@(AST.TypeName (AST.ScopedLocal l)) = Map.findWithDefault v l subst
  go v@(AST.TypeName (AST.ScopedGlobal _)) = v
  go v@(AST.TypeName (AST.ScopedAmbiguous _)) = v
  -- cannot apply generics to a generic variable anyway
  go (AST.TypeGeneric name args) = AST.TypeGeneric name (map go args)
  go (AST.TypeFunction ret args opts) = AST.TypeFunction (go ret) (map go args) (map go opts)

requireType :: MonadCompiler MessageContent m => TypeScheme -> TypeScheme -> m ()
requireType (TypeScheme gen1 ty1) (TypeScheme gen2 ty2)
  | length gen1 /= length gen2 = messageWithContext MessageError $ MessageGenericTypeMismatch
  | otherwise = compareTypes (substituteGenerics (Map.fromList $ zip gen1 (map (AST.TypeName . AST.ScopedLocal) gen2)) ty1) ty2
  where
    compareTypes t1@(AST.TypeName n1) t2@(AST.TypeName n2) =
      when (n1 /= n2) $ messageWithContext MessageError $ MessageTypesNotEqual t1 t2
    compareTypes t1@(AST.TypeGeneric n1 args1) t2@(AST.TypeGeneric n2 args2) = do
      when (n1 /= n2 || length args1 /= length args2) $
        messageWithContext MessageError $ MessageTypesNotEqual t1 t2
      zipWithM_ compareTypes args1 args2
    compareTypes t1@(AST.TypeFunction ret1 args1 opts1) t2@(AST.TypeFunction ret2 args2 opts2) = do
      compareTypes ret1 ret2
      when (length args1 /= length args2 || length opts1 /= length opts2) $
        messageWithContext MessageError $ MessageTypesNotEqual t1 t2
      zipWithM_ compareTypes args1 args2
      zipWithM_ compareTypes opts1 opts2
    compareTypes t1 t2 = messageWithContext MessageError $ MessageTypesNotEqual t1 t2

buildFunctionEnv :: Monad m => AST.FunSig AST.ScopedName -> StateT TypeEnv m ()
buildFunctionEnv sig = do
  requiredReturnType .= Just (TypeScheme [] $ sig ^. AST.funSigReturnType)
  forMOf_ (AST.funSigParameters . folded) sig $ \(AST.Param ty name _) ->
    typeMap . at (AST.ScopedLocal name) .= Just (TypeScheme [] ty)

populateGlobalTypeEnv :: Monad m => Map AST.ModuleName ScopedModule -> StateT TypeEnv m ()
populateGlobalTypeEnv = imapMOf_ (folded . scopedModuleVars . ifolded) $ \name sig ->
  case sig of
    ScopedFun fsig -> do
      let ret = fsig ^. AST.funSigReturnType
          args = toListOf (AST.funSigParameters . folded . filtered AST.isMandatory . AST.paramType) fsig
          opts = toListOf (AST.funSigParameters . folded . filtered AST.isOptional . AST.paramType) fsig
      typeMap . at name .= Just (TypeScheme (fsig ^. AST.funSigGenerics) (AST.TypeFunction ret args opts))
    ScopedVar vsig -> typeMap . at name .= Just (TypeScheme [] $ vsig ^. AST.varSigType)
