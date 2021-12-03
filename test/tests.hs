import SimpleCmd
import System.IO

program :: [String] -> IO ()
program args =
  putStrLn "" >> cmdLog "koji-install" ("-n" : "-b" : args)

tests :: [[String]]
tests =
  [["podman"]
  ,["-H", "https://kojihub.stream.centos.org/kojihub", "-d", "el9", "bash"]
  ,["-H", "stream", "-d", "el9", "kernel"]
--  ,["-H", "rpmfusion", "ffmpeg"]
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program tests
  putStrLn $ show (length tests) ++ " tests run"
