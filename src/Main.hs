-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra (forM_, when, whenJustM, (>=>))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.List.Extra (lower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime
import Network.HTTP.Directory
import Network.HTTP.Simple
import SimpleCmdArgs

data Snapshot = Latest | Newest | Snap String

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
    <*> snapOpt
  ]
  where
    snapOpt =
      Snap <$> strArg "SNAPSHOT" <|>
      flagWith Newest Latest 'l' "latest" "latest finished snapshot"

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

listCmd :: Maybe String -> IO ()
listCmd mrelease =
  getComposes mrelease >>= mapM_ T.putStrLn

getComposes :: Maybe String -> IO [Text]
getComposes mrelease = do
  let release = maybe "rawhide" lower mrelease
  httpDirectory' $ topUrl +/+ release

newestCompose :: Maybe String -> IO String
newestCompose mrelease = do
  composes <- getComposes mrelease
  return $ T.unpack $ last $ filter (not . (T.pack "latest-" `T.isPrefixOf`)) composes

statusCmd :: Bool -> Maybe String -> Snapshot -> IO ()
statusCmd debug mrelease snapshot = do
  let release = maybe "rawhide" lower mrelease
  fetchFile release
  where
    fetchFile release = do
      snap <-
        case snapshot of
          Latest -> return $ "latest-Fedora-" ++ capitalize release
          Newest -> newestCompose mrelease
          Snap snap -> return $ "Fedora-" ++ capitalize release ++ '-' : snap
      let snapurl = topUrl +/+ release +/+ snap
      when debug $ putStrLn snapurl
      forM_ ["COMPOSE_ID", "STATUS"] $ \file -> do
        resp <- parseRequest (snapurl +/+ file) >>= httpLBS
        B.putStr $ getResponseBody resp
        when (file == "COMPOSE_ID") $ putChar '\n'
      whenJustM (httpLastModified' (snapurl +/+ "STATUS")) $
        utcToLocalZonedTime >=> print

capitalize :: String -> String
capitalize "" = ""
capitalize (h:t) = toUpper h : t
