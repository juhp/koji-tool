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

data Words = Mine | Limit | Failure | Complete | Current | Build
  deriving (Enum,Bounded)

quickWords :: Words -> [String]
quickWords Mine = ["my","mine"]
quickWords Limit = ["last","latest"]
quickWords Failure = ["fail","failure","failed"]
quickWords Complete = ["complete","completed","completion",
                       "close","closed",
                       "finish","finished"]
quickWords Current = ["current", "building", "open"]
quickWords Build = ["build","builds"]

-- Package to choose build
-- FIXME: arch
-- FIXME: method
-- FIXME: user's
-- handle some extra words?
-- FIXME: unknown words are just ignored (should be package?)
quickCmd :: Maybe String -> Bool -> [String] -> IO ()
quickCmd _ _ [] = error' $ "use these known words:\n\n" ++ unlines (map (unwords . quickWords) [minBound..])
quickCmd mhub debug args = do
  let mine = if hasWord Mine then Just UserSelf else Nothing
      limit = if hasWord Limit then 1 else 10
      failure = hasWord Failure
      complete = hasWord Complete
      current = hasWord Current
      build = hasWord Build
  if build
    then
    let states = [BuildFailed|failure] ++ [BuildComplete|complete] ++
                 [BuildBuilding|current]
    in Builds.buildsCmd mhub mine limit states Nothing (Just "rpm") False debug Builds.BuildQuery
    else
    let states = [TaskFailed|failure] ++ [TaskClosed|complete] ++
                 [TaskOpen|current]
    in Tasks.tasksCmd mhub mine limit states [] Nothing Nothing False debug Nothing failure Tasks.TaskQuery
  where
    hasWord :: Words -> Bool
    hasWord word = any (`elem` quickWords word) args
