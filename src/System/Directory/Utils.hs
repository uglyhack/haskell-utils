module System.Directory.Utils
  ( getDirectoryContentsRecursive
  , getFilesRecursive
  , getDirectoriesRecursive
  , SomeException
  )
  where

import Control.Exception
  ( SomeException, catch)
import Control.Monad   
  ( foldM)
import Control.Monad.Writer
  ( lift, execWriterT, tell)
import System.Directory 
  ( getDirectoryContents, doesFileExist)
import System.FilePath
  ( (</>))

getDirectoryContentsRecursive :: (SomeException -> IO ([FilePath],[FilePath]))
                              -> FilePath
                              -> IO ([FilePath],[FilePath])
getDirectoryContentsRecursive handler = execWriterT . scanRecursive
  where
    scanRecursive d              = do
      (ds,fs) <- lift $ catch (getDirectoryContents d >>= separateEntries d)
                        handler
      tell (ds,fs)
      mapM_ scanRecursive ds
    separateEntries d            = foldM (separateEntry d) ([],[])
    separateEntry d es@(ds,fs) e =
      let e' = d </> e
      in  do isFile <- doesFileExist e'
             return $ if   (e == ".") || (e == "..")
                      then es
                      else if   isFile
                           then (ds,e':fs)
                           else (e':ds,fs)

getFilesRecursive handler dir =
  snd `fmap` getDirectoryContentsRecursive handler dir

getDirectoriesRecursive handler dir =
  fst `fmap` getDirectoryContentsRecursive handler dir
