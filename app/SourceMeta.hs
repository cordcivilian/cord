{-# LANGUAGE OverloadedStrings #-}

module SourceMeta where

import qualified Data.Aeson as JSON

import qualified Network.HTTP.Simple as HTTP

import qualified Data.Time as Time
import qualified Data.Time.Format.ISO8601 as ISO8601

data RepoInfo = RepoInfo
  { repoName :: String
  , repoHTML :: String
  , repoUpdatedAt :: String
  } deriving (Show)

instance JSON.FromJSON RepoInfo where
  parseJSON = JSON.withObject "RepoInfo" $ \v -> RepoInfo
    <$> v JSON..: "name"
    <*> v JSON..: "html_url"
    <*> v JSON..: "updated_at"

getRepoInfo :: String -> String -> IO (Either String RepoInfo)
getRepoInfo owner repo = do
  let url = "https://api.github.com/repos/" ++ owner ++ "/" ++ repo
  request <- HTTP.parseRequest url
  response <- HTTP.httpLBS $
    HTTP.addRequestHeader "User-Agent" "cordcivilian" request
  let jsonBody = HTTP.getResponseBody response
  return $ JSON.eitherDecode jsonBody

getUpdatedAt :: String -> IO String
getUpdatedAt s = do
    let (owner, repo) = getOwnerRepoFromSourceLink s
    result <- getRepoInfo owner repo
    case result of
        Right repoInfo -> do
            let maybeUpdatedAt = getISO8601Date $ repoUpdatedAt repoInfo -- pure
            return $ maybe "-unavailable" id maybeUpdatedAt
        _ -> return "-unavailable"

getISO8601Date :: String -> Maybe String
getISO8601Date iso8601 = do
    utcTime <- ISO8601.iso8601ParseM iso8601 :: Maybe Time.UTCTime
    return $ ISO8601.formatShow
      (ISO8601.calendarFormat ISO8601.ExtendedFormat) (Time.utctDay utcTime)

getOwnerRepoFromSourceLink :: String -> (String, String)
getOwnerRepoFromSourceLink s = extractTuple $ split s '/'

extractTuple :: [String] -> (String, String)
extractTuple (_:owner:repo:_) = (owner, repo)
extractTuple _ = ("", "")

split :: String -> Char -> [String]
split str delim =
  case break (==delim) str of
    (a, _:b) -> a : split b delim
    (a, _)   -> [a]
