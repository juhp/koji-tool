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

-- Package to choose build
-- FIXME: building
-- handle some extra words?
quickCmd :: Maybe String -> [String] -> IO ()
quickCmd _ [] = error' "please use words: 'my', 'last', 'fail', 'build'"
quickCmd mhub args = do
  let mine = if any (`elem` ["my","mine"]) args
             then Just UserSelf
             else Nothing
      limit = if any (`elem` ["last","latest"]) args
              then 1
              else 10
      failure = any (`elem` ["fail","failure","failed"]) args
      complete = any (`elem` ["complete","completed","close","closed",
                              "finish","finished"]) args
      build = any (`elem` ["build","builds"]) args
  if build
    then
    let states = [BuildFailed|failure] ++ [BuildComplete|complete]
    in Builds.buildsCmd mhub mine limit states Nothing (Just "rpm") False False Builds.BuildQuery
    else
    let states = [TaskFailed|failure] ++ [TaskClosed|complete]
    in Tasks.tasksCmd mhub mine limit states [] Nothing Nothing False False Nothing failure Tasks.TaskQuery
