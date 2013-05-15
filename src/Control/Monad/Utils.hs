module Control.Monad.Utils
  where

import Control.Monad
  ( Monad, foldM)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p = foldM (\es e -> p e >>= return . select es e) ([],[])
  where
    select (ts,fs) x r | r         = (x:ts,fs)
                       | otherwise = (ts,x:fs)
