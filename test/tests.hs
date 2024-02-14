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
     ,["-r", "updates"]
     ,["-l2", "updates"]
     ,["updates", "fedora-39"]
     ])
  ,
    ("status",
     [["updates", "-n"]
     ,["updates", "-n", "fedora-38"]
     ,["rawhide", "-L"]
     ,["branched", "-l2"]
     ,["branched", "-n", "39"]
     ])
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  mapM_ program tests
  putStrLn $ "\n" ++ show (length tests) ++ " command tests run"
