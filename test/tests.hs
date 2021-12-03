import SimpleCmd
import System.IO

program :: [String] -> IO ()
program args =
  putStrLn "" >> cmdLog "koji-install" ("-n" : args)

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
  mapM_ program tests
  putStrLn $ show (length tests) ++ " tests run"
