-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import Control.Monad.Extra (when, whenJustM, (>=>))
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char ( isDigit )
import Data.List.Extra ( lower, groupOn, sort, sortOn, takeEnd)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime (utcToLocalZonedTime)
import Network.HTTP.Directory
    ( (+/+), httpDirectory', httpLastModified', noTrailingSlash )
import Network.HTTP.Simple
    ( parseRequest, getResponseBody, httpLBS )
import SimpleCmdArgs

-- FIXME branched
main :: IO ()
main =
  simpleCmdArgs' Nothing
  "check status of fedora composes"
  "description here" $
  subcommands
  [ Subcommand "list"
    "List dirs/composes" $
    listCmd
    <$> debugOpt
    <*> numOpt
    <*> optional dirOpt
    <*> optional snapOpt
  , Subcommand "status"
    "Show compose status" $
    statusCmd
    <$> debugOpt
    <*> numOpt
    <*> dirOpt
    <*> optional snapOpt
  ]
  where
    debugOpt = switchWith 'd' "debug" "debug output"

    numOpt =
      flagWith' Nothing 'a' "all" "All composes" <|>
      Just <$> optionalWith auto 'n' "number" "LIMIT" "Number of composes (default: 1)" 1

    dirOpt = strArg "DIR"

    snapOpt = strArg "SUBSTR"

topUrl :: String
topUrl = "https://kojipkgs.fedoraproject.org/compose"

httpDirectories :: String -> IO [Text]
httpDirectories = fmap (map noTrailingSlash) . httpDirectory'

listCmd :: Bool -> Maybe Int -> Maybe String -> Maybe String -> IO ()
listCmd _ _ Nothing _ =
  httpDirectories topUrl >>= mapM_ T.putStrLn
listCmd debug mlimit (Just dir) mpat =
  getComposes debug mlimit dir mpat >>= mapM_ T.putStrLn

getComposes :: Bool -> Maybe Int -> FilePath -> Maybe String -> IO [Text]
getComposes debug mlimit dir mpat = do
  let url = topUrl +/+ dir
  when debug $ putStrLn url
  mconcat . map sort . limitNumber . groupOn (T.takeWhileEnd (/= '-')) . sortOn (T.takeWhileEnd (/= '-')) . subset .
    filter (\c -> isDigit (T.last c) && T.any (== '.') c) <$>
    httpDirectories url
  where
    subset = maybe id (\n -> filter ((T.pack (lower n) `T.isInfixOf`) . T.toLower)) mpat

    limitNumber = maybe id takeEnd mlimit

statusCmd :: Bool -> Maybe Int -> FilePath -> Maybe String -> IO ()
statusCmd debug mlim dir mpat =
  getComposes debug mlim dir mpat >>=
  mapM_ checkStatus
  where
    checkStatus compose = do
      let snapurl = topUrl +/+ dir +/+ T.unpack compose
      when debug $ putStrLn snapurl
      putComposeFile snapurl "STATUS"
      putChar ' '
      whenJustM (httpLastModified' (snapurl +/+ "STATUS")) $
        utcToLocalZonedTime >=> putStr . (++ " ") . show
      putComposeFile snapurl "COMPOSE_ID"
      putChar '\n'

    putComposeFile url file =
      parseRequest (url +/+ file) >>= httpLBS >>=
      B.putStr . removeFinalNewLine . getResponseBody


    removeFinalNewLine bs = if B.last bs == '\n' then B.init bs else bs

-- capitalize :: String -> String
-- capitalize "" = ""
-- capitalize (h:t) = toUpper h : t
