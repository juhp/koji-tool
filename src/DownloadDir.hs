module DownloadDir (
  setDownloadDir)
where

import Control.Monad
import SimpleCmd (error')
import System.Directory (createDirectoryIfMissing,
                         doesDirectoryExist, getHomeDirectory,
                         setCurrentDirectory)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath

-- FIXME check writeable
setDownloadDir :: Bool -> String -> IO FilePath
setDownloadDir dryrun subdir = do
  home <- getHomeDirectory
  dlDir <- getUserDir "DOWNLOAD"
  dirExists <- doesDirectoryExist dlDir
  -- is this really necessary?
  unless (dryrun || dirExists) $
    when (home == dlDir) $
      error' "HOME directory does not exist!"
  let filesDir = dlDir </> subdir
  filesExists <- doesDirectoryExist filesDir
  dir <-
    if filesExists
    then setCWD filesDir
    else
    if dirExists
      then setCWD dlDir
      else do
      if dryrun
        then return dlDir
        else do
        createDirectoryIfMissing True dlDir
        setCWD dlDir
  let path = makeRelative home dir
  return $ if isRelative path then "~" </> path else path
  where
    setCWD :: FilePath -> IO FilePath
    setCWD dir = do
      setCurrentDirectory dir
      return dir
