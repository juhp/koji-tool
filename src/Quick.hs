{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Quick (
  quickCmd
  )
where

import Distribution.Koji
import SimpleCmd (error')

import qualified Builds
import qualified Tasks
import User

data Words = Mine | Limit | Failure | Complete | Build
  deriving (Enum,Bounded)

quickWords :: Words -> [String]
quickWords Mine = ["my","mine"]
quickWords Limit = ["last","latest"]
quickWords Failure = ["fail","failure","failed"]
quickWords Complete = ["complete","completed","completion",
                       "close","closed",
                       "finish","finished"]
quickWords Build = ["build","builds"]

-- Package to choose build
-- FIXME: building
-- handle some extra words?
quickCmd :: Maybe String -> [String] -> IO ()
quickCmd _ [] = error' $ "use these known words:\n\n" ++ unlines (map (unwords . quickWords) [minBound..])
quickCmd mhub args = do
  let mine = if hasWord Mine then Just UserSelf else Nothing
      limit = if hasWord Limit then 1 else 10
      failure = hasWord Failure
      complete = hasWord Complete
      build = hasWord Build
  if build
    then
    let states = [BuildFailed|failure] ++ [BuildComplete|complete]
    in Builds.buildsCmd mhub mine limit states Nothing (Just "rpm") False False Builds.BuildQuery
    else
    let states = [TaskFailed|failure] ++ [TaskClosed|complete]
    in Tasks.tasksCmd mhub mine limit states [] Nothing Nothing False False Nothing failure Tasks.TaskQuery
  where
    hasWord :: Words -> Bool
    hasWord word = any (`elem` quickWords word) args
