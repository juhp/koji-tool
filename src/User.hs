module User (
  UserOpt(..),
  maybeGetKojiUser
    )
where

import Control.Monad
import Data.List.Extra
import Data.Maybe
import Distribution.Koji
import System.Directory (findExecutable)
import SimpleCmd

data UserOpt = User String | UserSelf
  deriving Eq

maybeGetKojiUser :: String -> Maybe UserOpt -> IO (Maybe UserID)
maybeGetKojiUser _server Nothing = return Nothing
maybeGetKojiUser server (Just useropt) = do
  user <-
    case useropt of
      User user -> return user
      UserSelf | server == fedoraKojiHub -> do
          mKlist <- findExecutable "klist"
          if isJust mKlist
            then do
            mkls <- fmap words <$> cmdMaybe "klist" ["-l"]
            case mkls of
              Nothing -> error "klist failed"
              Just kls ->
                case find ("@FEDORAPROJECT.ORG" `isSuffixOf`) kls of
                  Nothing -> error' "Could not determine FAS id from klist"
                  Just principal -> do
                    let user = dropSuffix "@FEDORAPROJECT.ORG" principal
                    putStrLn $ "user" +-+ user
                    return user
            else error' "Cannot determine Koji user: kerberos klist not installed - try --user"
      _ -> error' $ "Do not know how to determine Koji user for " ++ server
  muserid <- kojiGetUserID server user
  when (isNothing muserid) $ error' "userid not found"
  return muserid
