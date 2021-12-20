import SimpleCmd
import System.IO

program :: [String] -> IO ()
program args =
  putStrLn "" >>
  cmdLog "koji-install" ("-n" : args)

tests :: Bool -> [[String]]
tests havedist =
  [["podman", "-p", "podman"] ++ sysdist
  ,["-l", "coreutils"] ++ sysdist
  ,["-H", "https://kojihub.stream.centos.org/kojihub", "-d", "el9", "bash", "-p", "bash"]
  ,["-H", "stream", "-d", "el9", "kernel", "-x", "kernel-devel*", "-x", "*-debug*"]
  ,["-l", "-H", "stream", "-d", "el9", "grep"]
  ,["-H", "rpmfusion", "ffmpeg", "-p", "ffmpeg", "-p", "ffmpeg-libs"] ++ sysdist
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
