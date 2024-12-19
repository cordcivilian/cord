{-# LANGUAGE OverloadedStrings #-}

module SourceMeta where

import qualified Data.Aeson as JSON
import qualified Network.HTTP.Simple as HTTP

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
        Right repoInfo -> return $ repoUpdatedAt repoInfo
        Left _ -> return ""

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
