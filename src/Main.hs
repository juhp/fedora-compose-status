-- SPDX-License-Identifier: BSD-3-Clause
{-# Language LambdaCase #-}

module Main (main) where

import Control.Monad.Extra (foldM_, when)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char ( isDigit )
import Data.Functor ((<&>))
import Data.List.Extra (isPrefixOf, lower, nub, sort, takeEnd)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime)
import Network.HTTP.Directory
    ( (+/+), httpDirectory', httpExists', httpLastModified', noTrailingSlash )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )
import SimpleCmdArgs
import SimplePrompt
import System.Console.ANSI

import Paths_fedora_composes (version)

data Limit = NoLimit | Limit Int

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
    <*> optional limitOpt
    <*> switchWith 'r' "repos" "Only list target repos"
    <*> optional dirOpt
    <*> optional snapOpt
  , Subcommand "status"
    "Show compose status" $
    statusCmd
    <$> debugOpt
    <*> optional limitOpt
    <*> switchWith 'n' "no-more" "Do not prompt for more results"
    <*> dirOpt
    <*> optional snapOpt
  ]
  where
    debugOpt = switchWith 'd' "debug" "debug output"

    limitOpt =
      flagWith' NoLimit 'a' "all-composes" "All composes" <|>
      Limit <$> optionWith auto 'l' "limit" "LIMIT" "Max number of composes"

    dirOpt = strArg "DIR"

    snapOpt = strArg "SUBSTR"

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

httpDirectories :: String -> IO [Text]
httpDirectories = fmap (map noTrailingSlash) . httpDirectory'

listCmd :: Bool -> Maybe Limit -> Bool -> Maybe String
        -> Maybe String -> IO ()
listCmd _ _ _ Nothing _ =
  httpDirectories topUrl >>= mapM_ T.putStrLn
listCmd debug mlimit onlyrepos (Just dir) mpat =
  getComposes debug mlimit onlyrepos dir mpat >>= mapM_ putStrLn

data Compose =
  Compose {compDate :: Text, compRepo :: Text}
  deriving (Eq, Ord, Show)

readCompose :: Text -> Compose
readCompose t =
  case T.breakOnEnd (T.pack "-") t of
    (repoDash,date) -> Compose date (T.init repoDash)

showCompose :: Compose -> String
showCompose (Compose d r) = T.unpack r <> "-" <> T.unpack d

-- data RepoComposes = RepoComposes Text [Text]

getComposes :: Bool -> Maybe Limit -> Bool -> FilePath
            -> Maybe String -> IO [String]
getComposes debug mlimit onlyrepos dir mpat = do
  let url = topUrl +/+ dir
  when debug $ putStrLn url
  repocomposes <-
    limitComposes .
    sort .
    repoSubset .
    map readCompose .
    filter (\c -> isDigit (T.last c) && T.any (== '.') c) <$>
    httpDirectories url
  when debug $ print repocomposes
  return $ reverse $ selectRepos url repocomposes
  where
    selectRepos :: String -> [Compose] -> [String]
    selectRepos url =
      if onlyrepos
      then map T.unpack . nub . map compRepo
      else map ((url +/+) . showCompose)

    repoSubset :: [Compose] -> [Compose]
    repoSubset = maybe id (\n -> filter ((T.pack (lower n) `T.isInfixOf`) . T.toLower . compRepo)) mpat

    limitComposes =
      flip (maybe (takeEnd 10)) mlimit $
      \case
        Limit n -> takeEnd n
        NoLimit -> id

statusCmd :: Bool -> Maybe Limit -> Bool -> FilePath -> Maybe String
          -> IO ()
statusCmd debug mlimit nomore dir mpat = do
  tz <- getCurrentTimeZone
  getComposes debug mlimit False dir mpat >>=
    foldM_ (checkStatus tz) (Just False)
  where
    checkStatus tz mfinished snapurl =
      if mfinished == Just True
      then return (Just True)
      else do
        more <-
          if isNothing mfinished
          then
            if nomore
              then return False
              else do
                yes <- yesNoDefault False "Show more results"
                cursorUp 1 >> clearFromCursorToLineEnd
                return yes
          else return True
        if not more
          then return (Just True)
          else do
          -- FIXME use formatTime?
          mstart <- httpMaybeLastModified $  snapurl +/+ "COMPOSE_ID"
          mfinish <- httpMaybeLastModified $  snapurl +/+ "STATUS"
          status <-
            if isJust mfinish
            then B.unpack <$> getComposeFile snapurl "STATUS"
            else return "STATUS missing"
          putStrLn $ unlines $
            snapurl :
            [maybe "" show mstart | status /= "STARTED"] ++
            [maybe "" show mfinish ++ " " ++ status]
          return $ if "FINISHED" `isPrefixOf` status
                   then Nothing
                   else Just False
          where
            httpMaybeLastModified url = do
              exists <- httpExists' url
              if exists
                then fmap (utcToZonedTime tz) <$> httpLastModified' url
                else return Nothing

    getComposeFile url file =
      parseRequest (url +/+ file)
      >>= httpLBS
      <&> removeFinalNewLine . getResponseBody

    removeFinalNewLine bs = if B.last bs == '\n' then B.init bs else bs

-- capitalize :: String -> String
-- capitalize "" = ""
-- capitalize (h:t) = toUpper h : t
