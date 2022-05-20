import SimpleCmd
import System.IO

program :: ([String], [[String]]) -> IO ()
program (c, argsv) =
  putStrLn ("\n# " ++ head c) >>
  mapM_ run argsv
  where
    run args = do
      putStrLn ""
      cmdLog "koji-tool" (c ++ args)

tests :: Bool -> [([String], [[String]])]
tests havedist =
  [
    (["builds"],
     [["-L", "rust"]
     ,["-l", "3"]
     ,["-L", "-p", "rpm-ostree*.fc36"]])
  ,
    (["tasks"],
     [["-L"]
     ,["-l", "3"]
     ,["-L", "rpm-ostree"]])
  ,
    (["latest"],
     [["rawhide", "ghc"]])
  ,
    (["find"],
     [["last", "failed", "build"]])
  ,
        (["install", "-n", "-y"],
     [["podman", "-p", "podman"] ++ sysdist
     ,["-l", "coreutils"] ++ sysdist
     ,["-l", "-R", "rpmlint-2.2.0-1.fc36"]
     ,["-H", "https://kojihub.stream.centos.org/kojihub", "-d", "el9", "bash", "-p", "bash"]
     ,["-H", "stream", "-d", "el9", "kernel", "-x", "kernel-devel*", "-x", "*-debug*"]
     ,["-l", "-H", "stream", "-d", "el9", "grep"]
     ,["-H", "rpmfusion", "ffmpeg", "-p", "ffmpeg", "-p", "ffmpeg-libs"] ++ sysdist
     ,["-l", "-H", "rpmfusion", "ffmpeg"] ++ sysdist])
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
