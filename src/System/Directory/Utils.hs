module System.Directory.Utils
  ( getDirectoryContentsRecursive
  , getFilesRecursive
  , getDirectoriesRecursive
  , SomeException
  )
  where

import Control.Applicative
  ( (<$>))
import Control.Exception
  ( SomeException, handle)
import Control.Monad.Utils
  ( partitionM)
import Control.Monad.Writer
  ( lift, execWriterT, tell)
import System.Directory 
  ( getDirectoryContents, doesDirectoryExist)
import System.FilePath
  ( (</>))

getDirectoryContentsRecursive :: (SomeException -> IO ([FilePath],[FilePath]))
                              -> FilePath
                              -> IO ([FilePath],[FilePath])
getDirectoryContentsRecursive handler = execWriterT . scanRecursive
  where
    scanRecursive d = do
      (ds,fs) <- lift $   handle handler
                      $   map (d </>) . filter dotDirs
                      <$> getDirectoryContents d
                      >>= partitionM doesDirectoryExist
      tell (ds,fs)
      mapM_ scanRecursive ds
    dotDirs d       = not (d == "." || d == "..")

getFilesRecursive handler dir =
  snd <$> getDirectoryContentsRecursive handler dir

getDirectoriesRecursive handler dir =
  fst <$> getDirectoryContentsRecursive handler dir
