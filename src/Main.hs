-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra (when)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char ( isDigit )
import Data.Functor ((<&>))
import Data.List.Extra ( lower, groupOn, sort, sortOn, takeEnd)
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
    <*> numOpt
    <*> limitOpt
    <*> switchWith 'r' "repos" "Only list target repos"
    <*> optional dirOpt
    <*> optional snapOpt
  , Subcommand "status"
    "Show compose status" $
    statusCmd
    <$> debugOpt
    <*> numOpt
    <*> limitOpt
    <*> dirOpt
    <*> optional snapOpt
  ]
  where
    debugOpt = switchWith 'd' "debug" "debug output"

    numOpt =
      flagWith' Nothing 'a' "all-repos" "All repos" <|>
      Just <$> optionalWith auto 'n' "num" "NOREPOS" "Number of repos (default: 6)" 6

    limitOpt =
      flagWith' Nothing 'A' "all-composes" "All composes" <|>
      Just <$> optionalWith auto 'l' "limit" "LIMIT" "Number of composes (default: 1)" 1

    dirOpt = strArg "DIR"

    snapOpt = strArg "SUBSTR"

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

httpDirectories :: String -> IO [Text]
httpDirectories = fmap (map noTrailingSlash) . httpDirectory'

listCmd :: Bool -> Maybe Int -> Maybe Int -> Bool -> Maybe String
        -> Maybe String -> IO ()
listCmd _ _ _ _ Nothing _ =
  httpDirectories topUrl >>= mapM_ T.putStrLn
listCmd debug mrepos mlimit onlyrepos (Just dir) mpat =
  getComposes debug mrepos mlimit onlyrepos dir mpat >>= mapM_ T.putStrLn

getComposes :: Bool -> Maybe Int -> Maybe Int -> Bool -> FilePath
            -> Maybe String -> IO [Text]
getComposes debug mrepos mlimit onlyrepos dir mpat = do
  let url = topUrl +/+ dir
  when debug $ putStrLn url
  repocomposes <-
    groupOn (T.dropWhileEnd (/= '-')) .
    sortOn (T.dropWhileEnd (/= '-')) . subset .
    filter (\c -> isDigit (T.last c) && T.any (== '.') c) <$>
    httpDirectories url
  when debug $ print $ map last repocomposes
  return $
    (if onlyrepos
     then mconcat . map limitRepos . groupOn removeRelease . map removeDate
     else mconcat . map (sort . limitComposes) . limitRepos) repocomposes
  where
    subset = maybe id (\n -> filter ((T.pack (lower n) `T.isInfixOf`) . T.toLower)) mpat

    limitRepos = maybe id takeEnd mrepos
    limitComposes = maybe id takeEnd mlimit

    removeDate = T.init . T.dropWhileEnd (/= '-') . head

    removeRelease t =
      let reldash = (T.dropWhileEnd isDigit . T.dropWhileEnd (not . isDigit)) t
      in if T.null reldash
         then t
         else T.init reldash

-- FIXME sort output by timestamp
statusCmd :: Bool -> Maybe Int -> Maybe Int -> FilePath -> Maybe String
          -> IO ()
statusCmd debug mrepos mlimit dir mpat = do
  tz <- getCurrentTimeZone
  getComposes debug mrepos mlimit False dir mpat >>=
    mapM (checkStatus tz) >>= mapM_ putStrLn . sort
  where
    checkStatus tz compose = do
      let snapurl = topUrl +/+ dir +/+ T.unpack compose
      when debug $ putStrLn snapurl
      status <- getComposeFile snapurl "STATUS"
--      putChar ' '
      -- FIXME use formatTime
      mstart <- fmap (utcToZonedTime tz) <$>
                httpLastModified' (snapurl +/+ "COMPOSE_ID")
      mfinish <- fmap (utcToZonedTime tz) <$>
                 httpLastModified' (snapurl +/+ "STATUS")
      composeId <- getComposeFile snapurl "COMPOSE_ID"
      return $ unwords [maybe "" ((++ " ->") . show)  mstart,
                        maybe "" show mfinish,
                        B.unpack status,
                        B.unpack composeId]

    getComposeFile url file =
      parseRequest (url +/+ file)
      >>= httpLBS
      <&> removeFinalNewLine . getResponseBody


    removeFinalNewLine bs = if B.last bs == '\n' then B.init bs else bs

-- capitalize :: String -> String
-- capitalize "" = ""
-- capitalize (h:t) = toUpper h : t
