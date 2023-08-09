-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra (when)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char ( isDigit )
import Data.Functor ((<&>))
import Data.List.Extra ( lower, sort, sortOn, takeEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime)
import Network.HTTP.Directory
    ( (+/+), httpDirectory', httpLastModified', noTrailingSlash )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )
import SimpleCmdArgs

import Paths_fedora_composes (version)

main :: IO ()
main =
  simpleCmdArgs' (Just version)
  "check status of fedora composes"
  "description here" $
  subcommands
  [ Subcommand "list"
    "List dirs/composes (by default only last compose)" $
    listCmd
    <$> debugOpt
    <*> limitOpt
    <*> switchWith 'r' "repos" "Only list target repos"
    <*> optional dirOpt
    <*> optional snapOpt
  , Subcommand "status"
    "Show compose status" $
    statusCmd
    <$> debugOpt
    <*> limitOpt
    <*> dirOpt
    <*> optional snapOpt
  ]
  where
    debugOpt = switchWith 'd' "debug" "debug output"

    limitOpt =
      flagWith' Nothing 'a' "all-composes" "All composes" <|>
      Just <$> optionalWith auto 'l' "limit" "LIMIT" "Number of composes (default: 10)" 10

    dirOpt = strArg "DIR"

    snapOpt = strArg "SUBSTR"

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

httpDirectories :: String -> IO [Text]
httpDirectories = fmap (map noTrailingSlash) . httpDirectory'

listCmd :: Bool -> Maybe Int -> Bool -> Maybe String
        -> Maybe String -> IO ()
listCmd _ _ _ Nothing _ =
  httpDirectories topUrl >>= mapM_ T.putStrLn
listCmd debug mlimit onlyrepos (Just dir) mpat =
  getComposes debug mlimit onlyrepos dir mpat >>= mapM_ putStrLn

data Compose =
  Compose {compRepo :: Text, compDate :: Text}
  deriving Show

readCompose :: Text -> Compose
readCompose t =
  case T.breakOnEnd (T.pack "-") t of
    (repoDash,date) -> Compose (T.init repoDash) date

showCompose :: Compose -> Text
showCompose (Compose r d) = r <> T.pack "-" <> d

-- data RepoComposes = RepoComposes Text [Text]

getComposes :: Bool -> Maybe Int -> Bool -> FilePath
            -> Maybe String -> IO [String]
getComposes debug mlimit onlyrepos dir mpat = do
  let url = topUrl +/+ dir
  when debug $ putStrLn url
  repocomposes <-
    sortOn compDate .
    repoSubset .
    map readCompose .
    filter (\c -> isDigit (T.last c) && T.any (== '.') c) <$>
    httpDirectories url
  when debug $ print $ repocomposes
  return $
    map ((url +/+) . T.unpack) $ selectRepos repocomposes
  where
    selectRepos :: [Compose] -> [Text]
    selectRepos =
      if onlyrepos
      then map compRepo . limitComposes -- FIXME this is wrong
      else map showCompose . sortRelease . limitComposes

    repoSubset :: [Compose] -> [Compose]
    repoSubset = maybe id (\n -> filter ((T.pack (lower n) `T.isInfixOf`) . T.toLower . compRepo)) mpat

    limitComposes = maybe id takeEnd mlimit

    sortRelease = sortOn (T.takeWhileEnd (/= '-') . compRepo)

-- FIXME sort output by timestamp
statusCmd :: Bool -> Maybe Int -> FilePath -> Maybe String
          -> IO ()
statusCmd debug mlimit dir mpat = do
  tz <- getCurrentTimeZone
  getComposes debug mlimit False dir mpat >>=
    mapM (checkStatus tz) >>= mapM_ putStrLn . sort
  where
    checkStatus tz snapurl = do
      status <- getComposeFile snapurl "STATUS"
--      putChar ' '
      -- FIXME use formatTime
      mstart <- fmap (utcToZonedTime tz) <$>
                httpLastModified' (snapurl +/+ "COMPOSE_ID")
      mfinish <- fmap (utcToZonedTime tz) <$>
                 httpLastModified' (snapurl +/+ "STATUS")
      return $ unlines [snapurl,
                        maybe "" show mstart,
                        maybe "" show mfinish ++ " " ++ B.unpack status]

    getComposeFile url file =
      parseRequest (url +/+ file)
      >>= httpLBS
      <&> removeFinalNewLine . getResponseBody

    removeFinalNewLine bs = if B.last bs == '\n' then B.init bs else bs

-- capitalize :: String -> String
-- capitalize "" = ""
-- capitalize (h:t) = toUpper h : t
