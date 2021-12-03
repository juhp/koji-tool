import SimpleCmd
import System.IO

program :: Bool -> [String] -> IO ()
program havedist args =
  putStrLn "" >>
  cmdLog "koji-install"
  ((if havedist then id else (["-d", "fc35"] ++)) ("-n" : args))

tests :: [[String]]
tests =
  [["-b", "podman"]
  ,["-l", "coreutils"]
  ,["-b", "-H", "https://kojihub.stream.centos.org/kojihub", "-d", "el9", "bash"]
  ,["-b", "-H", "stream", "-d", "el9", "kernel"]
  ,["-l", "-H", "stream", "-d", "el9", "grep"]
  ,["-b", "-H", "rpmfusion", "ffmpeg"]
  ,["-l", "-H", "rpmfusion", "ffmpeg"]
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  havedist <- do
    dist <- cmd "rpm" ["--eval", "%{dist}"]
    return $ dist /= "%{dist}"
  mapM_ (program havedist) tests
  putStrLn $ show (length tests) ++ " tests run"
