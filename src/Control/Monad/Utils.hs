module Control.Monad.Utils
  where

import Control.Monad
  ( Monad, foldM)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p = foldM seperate ([],[])
  where
    seperate (ys,ns) x =
      p x >>= \r -> return $ if   r
                             then (x:ys,ns)
                             else (ys,x:ns)
