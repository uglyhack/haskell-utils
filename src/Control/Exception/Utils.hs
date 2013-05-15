module Control.Exception.Utils 
  ( ignoreException
  )
  where

import Control.Exception
  ( SomeException)
import Control.Monad.Trans
  ( MonadIO(..))

ignoreException :: MonadIO m => a -> SomeException -> m a
ignoreException a _ = liftIO $ return a
