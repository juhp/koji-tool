module DownloadDir (
  setDownloadDir
  )
where

import Control.Monad
import System.Directory (createDirectoryIfMissing, getHomeDirectory,
                         setCurrentDirectory)
import System.Environment.XDG.UserDir (getUserDir)
import System.FilePath

-- FIXME check writeable
setDownloadDir :: Bool -> String -> IO (IO ())
setDownloadDir dryrun subdir = do
  home <- getHomeDirectory
  dlDir <- getUserDir "DOWNLOAD"
  let dir = dlDir </> subdir
  unless dryrun $ do
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
  let path = makeRelative home dir
  return $
    putStrLn $
    "Packages downloaded to " ++
    if isRelative path then "~" </> path else path
