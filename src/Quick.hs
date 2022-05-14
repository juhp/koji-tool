{-# LANGUAGE CPP #-}

-- SPDX-License-Identifier: BSD-3-Clause

module Quick (
  quickCmd
  )
where

import Data.List.Extra ((\\), dropSuffix, isSuffixOf)
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

allWords :: [String]
allWords = concatMap quickWords [minBound..]

-- FIXME: arch
-- FIXME: method
quickCmd :: Maybe String -> Bool -> [String] -> IO ()
quickCmd _ _ [] = error' $ "quick handles these words:\n\n" ++
                  unlines
                  (map (unwords . quickWords) [minBound..] ++
                  ["PACKAGE", "USER's"])
quickCmd mhub debug args = do
  let user = if hasWord Mine
             then Just UserSelf
             else case filter ("'s" `isSuffixOf`) args of
                    [] -> Nothing
                    [users] -> Just $ User (dropSuffix "'s" users)
                    more -> error' $ "more than one user's given: " ++
                            unwords more
      limit = if hasWord Limit then 1 else 10
      failure = hasWord Failure
      complete = hasWord Complete
      current = hasWord Current
      build = hasWord Build
      mpkg =
        case removeUsers (args \\ allWords) of
          [] -> Nothing
          -- FIXME check valid package name (allow pattern?)
          [pkg] -> Just pkg
          other ->
            error' $
            "you can only specify one package - too many unknown words: " ++
            unwords other
  if build
    then
    let states = [BuildFailed|failure] ++ [BuildComplete|complete] ++
                 [BuildBuilding|current]
        buildreq = maybe Builds.BuildQuery Builds.BuildPackage mpkg
    in Builds.buildsCmd mhub user limit states Nothing (Just "rpm") False debug buildreq
    else
    let states = [TaskFailed|failure] ++ [TaskClosed|complete] ++
                 [TaskOpen|current]
        taskreq = maybe Tasks.TaskQuery Tasks.Package mpkg
    in Tasks.tasksCmd mhub user limit states [] Nothing Nothing False debug Nothing failure taskreq
  where
    hasWord :: Words -> Bool
    hasWord word = any (`elem` quickWords word) args

    removeUsers :: [String] -> [String]
    removeUsers = filter (not . ("'s" `isSuffixOf`))
