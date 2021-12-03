import SimpleCmd
import System.IO

program :: [String] -> IO ()
program args =
  putStrLn "" >>
  cmdLog "koji-install" ("-n" : args)

tests :: Bool -> [[String]]
tests havedist =
  [["-b", "podman"] ++ sysdist
  ,["-l", "coreutils"] ++ sysdist
  ,["-b", "-H", "https://kojihub.stream.centos.org/kojihub", "-d", "el9", "bash"]
  ,["-b", "-H", "stream", "-d", "el9", "kernel"]
  ,["-l", "-H", "stream", "-d", "el9", "grep"]
  ,["-b", "-H", "rpmfusion", "ffmpeg"] ++ sysdist
  ,["-l", "-H", "rpmfusion", "ffmpeg"] ++ sysdist
  ]
  where
    sysdist = if havedist then [] else ["-d", "fc35"]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  havedist <- do
    dist <- cmd "rpm" ["--eval", "%{dist}"]
    return $ dist /= "%{dist}"
  let cases = tests havedist
  mapM_ program cases
  putStrLn $ show (length cases) ++ " tests run"
