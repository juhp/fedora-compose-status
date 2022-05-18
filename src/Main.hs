-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra (forM_, when, whenJustM, (>=>))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.List.Extra (lower, takeEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime
import Network.HTTP.Directory
import Network.HTTP.Simple
import SimpleCmdArgs

data Snapshot = Latest | Newest | Snap String | Compose Text | Recent
  deriving Eq

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
      flagWith' Latest 'l' "latest" "latest finished snapshot" <|>
      flagWith Newest Recent 'r' "recent" "last 10 snapshots"

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

listCmd :: Maybe String -> IO ()
listCmd mrelease =
  getComposes mrelease >>= mapM_ T.putStrLn

getComposes :: Maybe String -> IO [Text]
getComposes mrelease = do
  let release = maybe "rawhide" lower mrelease
  filter (not . (T.pack "latest-" `T.isPrefixOf`)) <$>
    httpDirectory' (topUrl +/+ release)

newestCompose :: Maybe String -> IO String
newestCompose mrelease = do
  composes <- getComposes mrelease
  return $ T.unpack $ last composes

statusCmd :: Bool -> Maybe String -> Snapshot -> IO ()
statusCmd debug mrelease snapshot = do
  let release = maybe "rawhide" lower mrelease
  if snapshot == Recent
    then
    getComposes mrelease >>=
    mapM_ (statusCmd debug mrelease . Compose) . takeEnd 10
    else checkStatus release
  where
    checkStatus release = do
      snap <-
        case snapshot of
          Latest -> return $ "latest-Fedora-" ++ capitalize release
          Snap snap -> return $ "Fedora-" ++ capitalize release ++ '-' : snap
          Compose comp -> return $ T.unpack comp
          _ -> newestCompose mrelease
      let snapurl = topUrl +/+ release +/+ snap
      when debug $ putStrLn snapurl
      forM_ ["COMPOSE_ID", "STATUS"] $ \file -> do
        resp <- parseRequest (snapurl +/+ file) >>= httpLBS
        B.putStr $ removeFinalNewLine $ getResponseBody resp
        putChar ' '
      whenJustM (httpLastModified' (snapurl +/+ "STATUS")) $
        utcToLocalZonedTime >=> print

    removeFinalNewLine bs = if B.last bs == '\n' then B.init bs else bs
capitalize :: String -> String
capitalize "" = ""
capitalize (h:t) = toUpper h : t
