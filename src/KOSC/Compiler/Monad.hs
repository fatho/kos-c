{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
module KOSC.Compiler.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Monoid

import           Text.PrettyPrint.ANSI.Leijen as PP hiding ((<$>), (<>))

-- * Compiler Errors

data MessageType = MessageError | MessageWarning deriving (Eq, Ord, Enum, Bounded)

-- | A compiler error message together with a context.
data CompilerMessage a = CompilerMessage
  { _messageType    :: MessageType
  , _messageContext :: CompilerContext  -- ^ the context in which the message occurred
  , _messageContent :: a     -- ^ the actual message
  }

-- * Compiler Monad

-- | The context that is passed as an environment through the compiler
data CompilerContext = CompilerContext
  { _compilerContext :: [PP.Doc] -- ^ the stack of things the compiler is currently working on
  }

data CompilerState a = CompilerState
  { _compilerStateMessages :: [CompilerMessage a]
  }

-- | The compiler Monad transformer providing a context and error handling
newtype CompilerT e m a = CompilerT { runCompilerT' :: ReaderT CompilerContext (ExceptT (CompilerMessage e) (StateT (CompilerState e) m)) a  }
  deriving (Functor, Applicative, Monad, MonadError (CompilerMessage e))

makeLenses ''CompilerMessage
makeLenses ''CompilerContext
makeLenses ''CompilerState

runCompilerT :: Monad m => CompilerT e m a -> m (Either (CompilerMessage e) a, [CompilerMessage e])
runCompilerT compiler = over _2 _compilerStateMessages <$> runStateT (runExceptT (runReaderT (runCompilerT' compiler) emptyContext)) emptyState where
  emptyContext = CompilerContext []
  emptyState = CompilerState []

class Monad m => MonadCompiler e m | m -> e where
  -- | Throws a critical error that aborts the remainder of the compilation process.
  -- It is constructed from the given message and the context currently known to the compiler.
  criticalWithContext :: e -> m a

  -- | Aborts the computation if errors have been emitted previously
  cancelIfErrorneous :: e -> m ()

  -- | Gets all compiler messages
  getMessages :: m [CompilerMessage e]

  -- | Returns the current context of the compiler
  getContext :: m CompilerContext

  -- | Emits a compiler message but proceeds with the computation.
  messageWithContext :: MessageType -> e -> m ()

  -- | Locally modifies the context.
  localContext :: (CompilerContext -> CompilerContext) -> m a -> m a

  -- | Makes the current module known to sub-computations.
  pushContext :: PP.Doc -> m a -> m a
  pushContext ctx inModule = localContext (compilerContext %~ (ctx:)) inModule

instance Monad m => MonadCompiler e (CompilerT e m) where
  criticalWithContext msg = CompilerT $ ask >>= \ctx -> throwError (CompilerMessage MessageError ctx msg)

  cancelIfErrorneous msg = do
    msgs <- getMessages
    when (any (\m -> view messageType m == MessageError) msgs) $
      criticalWithContext msg

  getMessages = CompilerT $ use compilerStateMessages

  getContext = CompilerT ask

  messageWithContext ty content = CompilerT $ ask >>= \ctx -> compilerStateMessages %= (:) (CompilerMessage ty ctx content)

  localContext f = CompilerT . local f . runCompilerT'


instance MonadCompiler e m => MonadCompiler e (StateT s m) where
  criticalWithContext = lift . criticalWithContext
  cancelIfErrorneous = lift . cancelIfErrorneous
  getMessages = lift getMessages
  getContext = lift getContext
  messageWithContext t m = lift $ messageWithContext t m
  localContext f act = StateT $ \s -> localContext f (runStateT act s)

instance PP.Pretty e => PP.Pretty (CompilerMessage e) where
  pretty err = pretty (err ^. messageType) <> colon <+> pretty (err ^. messageContent) PP.<$$>
    indent 2 (vcat $ err ^. messageContext . compilerContext)

instance Pretty MessageType where
  pretty MessageError   = text "Error"
  pretty MessageWarning = text "Warning"

instance MonadTrans (CompilerT e) where
  lift act = CompilerT $ lift $ lift $ lift $ act

instance MonadIO m => MonadIO (CompilerT e m) where
  liftIO = lift . liftIO
