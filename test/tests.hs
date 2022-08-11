import SimpleCmd
import System.IO

program :: (String, [[String]]) -> IO ()
program (c, argsv) =
  putStrLn ("\n# " ++ c) >>
  mapM_ run argsv
  where
    run args = do
      putStrLn ""
      cmdLog "fedora-composes" (c : args)

tests :: [(String, [[String]])]
tests =
  [
    ("list",
     [["rawhide"]
     ,["-r", "-n4", "updates"]
     ,["-l2", "updates"]
     ,["updates", "fedora-36"]
     ])
  ,
    ("status",
     [["updates"]
     ,["updates", "fedora-36"]
     ,["rawhide"]
     ,["branched"]
     ,["branched", "37"]
     ])
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program tests
  putStrLn $ "\n" ++ show (length tests) ++ " command tests run"
