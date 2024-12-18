{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.IORef as IOR

import qualified Control.Monad.Trans as MTrans

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R

application :: Int -> Wai.Application
application state request respond =
    respond $ case (request_method, request_path) of
        ("GET", "/") -> rootTemplateRoute request
        ("GET", "/state") -> getState request state
        -- ("POST", "/state") -> incrementState request
        _ -> notFoundTemplateRoute request
        where
            request_method = BS.unpack $ Wai.requestMethod request
            request_path = BS.unpack $ Wai.rawPathInfo request

-- statefulMiddleware :: IOR.IORef Int -> Wai.Middleware
statefulMiddleware :: (MTrans.MonadIO m, Num t1, Show t1) => IOR.IORef t1 -> (t1 -> t2 -> t3 -> m b) -> t2 -> t3 -> m b
statefulMiddleware serverState app request respond = do
    count <- MTrans.liftIO $ incCount serverState
    app count request respond

monolith :: IOR.IORef Int -> Wai.Application
monolith s = Mid.logStdout $ statefulMiddleware s application

incCount :: (Num a, Show a) => IOR.IORef a -> IO a
incCount counter = IOR.atomicModifyIORef counter (\c -> (c+1, c))

data ServerState = ServerState { sState :: Int }

-- return current value
getState :: Wai.Request -> Int -> Wai.Response
getState _ state = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack $ show state)
    
-- return successful/failed: current value
incrementState :: Wai.Request -> Wai.Response
incrementState _ = Wai.responseLBS
    HTTP.status200
    [(Headers.hContentType, BS.pack "text/plain")]
    (BSL.pack "")

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
                H.a H.! A.href "#what-if" $ "what if ..."
                -- H.span " /// "
                -- H.span "healthcheck"
                -- H.span " /// "
                -- H.span "site stats"
        H.span H.! A.id "what-if" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "what if ..."
            whatIfTemplate $ WhatIf
                (WhatIfQuestion "... abc abc bac?")
                (WhatIfURL "abc.cordcivilian.com")
                (WhatIfSource "github.com/cordcivilian/abc")
                (WhatIfLastModified "2024-12-17")
            H.hr
            whatIfTemplate $ WhatIf
                (WhatIfQuestion "... third second forth?")
                (WhatIfURL "second.cordcivilian.com")
                (WhatIfSource "github.com/cordcivilian/second")
                (WhatIfLastModified "2024-12-01")

newtype WhatIfQuestion = WhatIfQuestion { unQuestion :: String }
newtype WhatIfURL = WhatIfURL { unURL :: String }
newtype WhatIfSource = WhatIfSource { unSource :: String }
newtype WhatIfLastModified = WhatIfLastModified {unLastModified :: String}

data WhatIf = WhatIf 
    { question :: WhatIfQuestion
    , url :: WhatIfURL
    , source :: WhatIfSource
    , lastModified :: WhatIfLastModified
    }

whatIfTemplate :: WhatIf -> H.Html
whatIfTemplate w = H.div $ do
    H.p H.! A.class_ "what-if-q" $ H.string (unQuestion $ question w)
    H.div H.! A.class_ "what-if-links" $ do
        H.a H.! A.class_ "what-if-link-site" H.!
            A.href (H.toValue $ "https://" ++ (unURL $ url w)) $
                H.string (unURL $ url w)
        H.span "|"
        H.a H.! A.class_ "what-if-link-source" H.!
            A.href (H.toValue $ "https://" ++ (unSource $ source w)) $
                H.string $ "source - v" ++ (unLastModified $ lastModified w)

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
    counter <- IOR.newIORef 0
    Warp.run port $ monolith counter
