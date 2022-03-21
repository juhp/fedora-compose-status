-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.List.Extra (lower)
import Data.Maybe
import qualified Data.Text.IO as T
import Network.HTTP.Directory
import Network.HTTP.Simple
import SimpleCmdArgs

-- FIXME branched
main :: IO ()
main =
  simpleCmdArgs' Nothing
  "check status of fedora composes"
  "description here" $
  subcommands
  [ Subcommand "list"
    "List composes" $
    listCmd
    <$> optional (strArg "RELEASE")
  , Subcommand "status"
    "Show compose status" $
    statusCmd
    <$> switchWith 'd' "debug" "debug output"
    <*> optional (strArg "RELEASE")
    <*> optional (strArg "SNAPSHOT")
  ]

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

listCmd :: Maybe String -> IO ()
listCmd mrelease = do
  let release = fromMaybe "rawhide" mrelease
  composes <- httpDirectory' $ topUrl +/+ release
  mapM_ T.putStrLn composes

statusCmd :: Bool -> Maybe String -> Maybe String -> IO ()
statusCmd debug mrelease msnapshot = do
  let release = maybe "rawhide" lower mrelease
  mapM_ (fetchFile release) ["COMPOSE_ID", "STATUS"]
  where
    fetchFile release file = do
      let snapshot =
            case msnapshot of
              Nothing -> "latest-Fedora-" ++ capitalize release
              Just snap ->  "Fedora-" ++ capitalize release ++ '-' : snap
          url = topUrl +/+ release +/+ snapshot +/+ file
      when debug $ putStrLn url
      resp <- parseRequest url
            >>= httpLBS
      B.putStrLn $ getResponseBody resp

capitalize :: String -> String
capitalize "" = ""
capitalize (h:t) = toUpper h : t
