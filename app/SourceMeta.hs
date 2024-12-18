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
