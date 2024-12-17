{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R

app :: Wai.Application
app request respond = do
    -- body <- Wai.lazyRequestBody request
    respond $ case (request_method, request_path) of
        ("GET", "/") -> rootTemplateRoute request
        _ -> notFoundTemplateRoute request
        where
            request_method = BS.unpack $ Wai.requestMethod request
            request_path = BS.unpack $ Wai.rawPathInfo request

loggedApp :: Wai.Application
loggedApp = Mid.logStdout app

cssEntry :: T.Text -> [T.Text] -> T.Text
cssEntry selector properties = T.unlines
    [ selector <> " {"
    , T.intercalate "\n" (map (\p -> "    " <> p <> ";") properties)
    , "}"
    ]

cssProperty :: T.Text -> T.Text -> T.Text
cssProperty property value = T.intercalate ": " [property, value]

combineCSS :: [T.Text] -> T.Text
combineCSS = T.intercalate ""

rootCSS :: T.Text
rootCSS = cssEntry ":root" 
    [ cssProperty "color-scheme" "light dark"
    ]

bodyHtmlCSS :: T.Text
bodyHtmlCSS = cssEntry "body, html"
    [ cssProperty "margin" "0 auto"
    , cssProperty "padding" "0 50px"
    , cssProperty "font-family" "'Lucida Console', monospace"
    , cssProperty "font-size" "18px"
    , cssProperty "max-width" "1280px"
    ]

frameCSS :: T.Text
frameCSS = cssEntry "#frame"
    [ cssProperty "min-height" "100vh"
    , cssProperty "min-height" "100dvh"
    , cssProperty "text-align" "center"
    , cssProperty "align-content" "center"
    , cssProperty "border" "3px"
    , cssProperty "box-sizing" "border-box"
    , cssProperty "background" "light-dark(white, black)"
    , cssProperty "color" "light-dark(black, white)"
    ]

linkCSS :: T.Text
linkCSS = cssEntry "a"
    [ cssProperty "text-decoration" "none"
    , cssProperty "color" "#4169e1"
    ]

hrCSS :: T.Text
hrCSS = cssEntry "hr"
    [ cssProperty "border" "none"
    , cssProperty "height" "2px"
    , cssProperty "background-color" "lightgrey"
    , cssProperty "margin" "30px auto"
    ]

whatIfQuestionCSS :: T.Text
whatIfQuestionCSS = cssEntry ".what-if-q"
    [ cssProperty "text-align" "left"
    , cssProperty "" ""
    ]

whatIfLinksCSS :: T.Text
whatIfLinksCSS = cssEntry ".what-if-links"
    [ cssProperty "display" "flex"
    ]

whatIfLinkSiteCSS :: T.Text
whatIfLinkSiteCSS = cssEntry ".what-if-link-site"
    [ cssProperty "flex" "60%"
    ]

whatIfLinkSourceCSS :: T.Text
whatIfLinkSourceCSS = cssEntry ".what-if-link-source"
    [ cssProperty "flex" "40%"
    ]

fullCSS :: T.Text
fullCSS = combineCSS
    [ rootCSS
    , bodyHtmlCSS
    , frameCSS
    , linkCSS
    , hrCSS
    , whatIfQuestionCSS
    , whatIfLinksCSS
    , whatIfLinkSourceCSS
    , whatIfLinkSiteCSS
    ]

rootTemplateRoute :: Wai.Request -> Wai.Response
rootTemplateRoute _ = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml rootTemplate)

rootTemplate :: H.Html
rootTemplate = H.docTypeHtml $ H.html $ do
    H.head $ do
        H.title "eta: 0 mins"
        H.style $ H.text fullCSS
    H.body $ do
        H.span H.! A.id "top" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "internet common #5819574234"
            H.div $ do
                H.a H.! A.class_ "link" H.! A.href "#what-if" $ "what if ..."
                -- H.span " /// "
                -- H.span "healthcheck"
                -- H.span " /// "
                -- H.span "site stats"
        H.span H.! A.id "what-if" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "what if ..."
            whatIfTemplate
                "... abc abc bac?"
                "abc.cordcivilian.com"
                "github.com/cordcivilian/abc"
                "2024-12-17"
            H.hr
            whatIfTemplate
                "... third second forth?"
                "second.cordcivilian.com"
                "github.com/cordcivilian/second"
                "2024-12-01"

whatIfTemplate :: String -> String -> String -> String -> H.Html
whatIfTemplate question url source modified = H.div $ do
    H.p H.! A.class_ "what-if-q" $ H.string question
    H.div H.! A.class_ "what-if-links" $ do
        H.a H.! A.class_ "what-if-link-site" H.!
            A.href (H.toValue $ "https://" ++ url) $
                H.string url
        H.span " | "
        H.a H.! A.class_ "what-if-link-source" H.!
            A.href (H.toValue $ "https://" ++ source) $
                H.string $ "source - v" ++ modified

notFoundTemplateRoute :: Wai.Request -> Wai.Response
notFoundTemplateRoute _ = Wai.responseLBS
    HTTP.status404
    [(Headers.hContentType, BS.pack "text/html")]
    (R.renderHtml notFoundTemplate)

notFoundTemplate :: H.Html
notFoundTemplate = H.docTypeHtml $ H.html $ do
    H.head $ do
        H.title "error"
        H.style $ I.preEscapedText fullCSS
    H.body $ do
        H.div H.! A.id "frame" $ do
            H.h1 "404 - not found"
            H.h2 $ do
               H.a H.! A.class_ "link" H.! A.href "/" $ "home"

main :: IO ()
main = do
    let port = 5000
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    Warp.run port loggedApp
