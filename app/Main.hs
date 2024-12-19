{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Data.Map as M

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import qualified Data.IORef as IOR

import qualified Control.Monad as Monad
-- import qualified Control.Monad.Trans as MTrans

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Types.Header as Headers

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as Mid

import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as R

import qualified SourceMeta as Source

application :: IOR.IORef WhatIfsConfig -> Wai.Application
application statesRef request respond =
    case (request_method, request_path) of
        ("GET", "/") -> do
            response <- rootTemplateRoute request statesRef
            respond response
        ("GET", "/test/states") -> do
            response <- statesRoute request statesRef
            respond response
        _ -> respond $ notFoundTemplateRoute request
        where
            request_method = BS.unpack $ Wai.requestMethod request
            request_path = BS.unpack $ Wai.rawPathInfo request

statefulMiddleware :: IOR.IORef WhatIfsConfig
                   -> (IOR.IORef WhatIfsConfig -> t1 -> t2 -> IO b)
                   -> t1 -> t2 -> IO b
statefulMiddleware statesRef app request respond = do
    _ <- updateSourceLastModified statesRef
    app statesRef request respond

updateSourceLastModified :: IOR.IORef WhatIfsConfig -> IO WhatIfsConfig
updateSourceLastModified statesRef = do
    currentConfig <- IOR.readIORef statesRef
    updatedList <- Monad.forM currentConfig $ \(s, (q, u, status, mLastModified)) -> do
        case mLastModified of
            Nothing -> do
                l <- Source.getUpdatedAt s
                case l of 
                  "-unavailable" -> return (s, (q, u, status, Nothing))
                  _ -> return (s, (q, u, status, Just $ WhatIfLastModified l))
            Just existing -> return (s, (q, u, status, Just $ existing))
    IOR.atomicWriteIORef statesRef updatedList
    return updatedList

monolith :: IOR.IORef WhatIfsConfig -> Wai.Application
monolith statesRef = Mid.logStdout $ statefulMiddleware statesRef application

statesRoute :: Wai.Request -> IOR.IORef WhatIfsConfig -> IO Wai.Response
statesRoute _ statesRef = do
    states <- IOR.readIORef statesRef
    return $ Wai.responseLBS
        HTTP.status200
        [(HTTP.hContentType, "text/plain")]
        (BSL.pack $ displayStates states)
            where
                displayStates :: WhatIfsConfig -> String
                displayStates [] = ""
                displayStates (x:xs) = show x ++ "\n" ++ displayStates xs


rootTemplateRoute :: Wai.Request -> IOR.IORef WhatIfsConfig -> IO Wai.Response
rootTemplateRoute _ statesRef = do
    states <- IOR.readIORef statesRef
    return $ Wai.responseLBS
        HTTP.status200
        [(Headers.hContentType, BS.pack "text/html")]
        (R.renderHtml $ rootTemplate states)

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
    , cssProperty "font-size" "20px"
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
    [ cssProperty "" ""
    , cssProperty "" ""
    ]

whatIfSourceCSS :: T.Text
whatIfSourceCSS = cssEntry ".what-if-s"
    [ cssProperty "font-family" "Optima, serif"
    , cssProperty "font-weight" "600"
    ]

fullCSS :: T.Text
fullCSS = combineCSS
    [ rootCSS
    , bodyHtmlCSS
    , frameCSS
    , linkCSS
    , hrCSS
    , whatIfQuestionCSS
    , whatIfSourceCSS
    ]

rootTemplate :: WhatIfsConfig -> H.Html
rootTemplate states = H.docTypeHtml $ H.html $ do
    H.head $ do
        H.title "eta: 0 mins"
        H.style $ H.text fullCSS
    H.body $ do
        H.span H.! A.id "top" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "internet common #5819574234"
            H.div $ do
                H.a H.! A.href "#what-if" $ "what if ..."
        H.span H.! A.id "what-if" $ ""
        H.div H.! A.id "frame" $ do
            H.h1 "what if ..."
            mkWhatIfs $ whatIfs states

mkWhatIfs :: [H.Html] -> H.Html
mkWhatIfs [] = ""
mkWhatIfs (x:[]) = x
mkWhatIfs (x:xs) = x >> H.hr >> mkWhatIfs xs

mkWhatIf :: WhatIfQuestion
         -> WhatIfProjectStatus 
         -> WhatIfURL
         -> WhatIfSource
         -> Maybe WhatIfLastModified
         -> H.Html
mkWhatIf a b c d e = whatIfTemplate b (WhatIf a c d e)

whatIfs :: WhatIfsConfig -> [H.Html]
whatIfs states =
    let go = (\(k, (q, u, s, m)) -> mkWhatIf (WhatIfQuestion q) s (WhatIfURL u) (WhatIfSource k) m)
    in map go states

data WhatIfProjectStatus = Released | WIP | Announced
    deriving (Eq, Show, Enum, Bounded)
data WhatIfLastModified = WhatIfLastModified { unLastModified :: String }
    deriving (Show)

newtype WhatIfQuestion = WhatIfQuestion { unQuestion :: String }
newtype WhatIfURL = WhatIfURL { unURL :: String }
newtype WhatIfSource = WhatIfSource { unSource :: String }

data WhatIf = WhatIf 
    { question :: WhatIfQuestion
    , url :: WhatIfURL
    , source :: WhatIfSource
    , lastModified :: Maybe WhatIfLastModified
    }

whatIfTemplate :: WhatIfProjectStatus -> WhatIf -> H.Html
whatIfTemplate status t = case status of 
    Released -> H.div $ do
        H.p H.! A.class_ "what-if-q" $ do
            H.a H.! A.href (H.toValue $ "https://" ++ (unURL $ url t)) $
                H.i $ H.string (unQuestion $ question t)
        H.p H.! A.class_ "what-if-s" $ do
            H.a H.! A.href (H.toValue $ "https://" ++ (unSource $ source t)) $
                H.string $ showWhatIfLastModified $ lastModified t
    WIP -> H.div $ do
        H.p H.! A.class_ "what-if-q" $ do
            H.i $ H.string $ "( " ++ (unQuestion $ question t) ++ " )"
        H.p H.! A.class_ "what-if-s" $ do
            H.a H.! A.href (H.toValue $ "https://" ++ (unSource $ source t)) $
                H.string $ showWhatIfLastModified $ lastModified t
    Announced -> H.div $ do
        H.p H.! A.class_ "what-if-q" $ do
            H.i $ H.string $ "( " ++ (unQuestion $ question t) ++ " )"

showWhatIfLastModified :: Maybe WhatIfLastModified -> String
showWhatIfLastModified a =
    case a of 
      Just t -> "[ v" ++ unLastModified t ++ " ]"
      Nothing -> "[ source ]"

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
            H.h1 $ do
               H.a H.! A.class_ "link" H.! A.href "/" $ "home"

type WhatIfsConfig = [(String, (String, String, WhatIfProjectStatus, Maybe WhatIfLastModified))]
projects :: WhatIfsConfig
projects = 
    [ ("github.com/cordcivilian/cord", ("websites are cool again?", "www.cordcivilian.com", Released, Nothing))
    , ("github.com/cordcivilian/walden99-run", ("walden99 walden99 walden99?", "run.walden99.com", WIP, Nothing))
    , ("github.com/cordcivilian/binary", ("binary binary binary?", "binary.cordcivilian.com", Announced, Nothing))
    ]

main :: IO ()
main = do
    let port = 5000
    putStrLn $ "Server starting on port " ++ show (port :: Int)
    ini <- IOR.newIORef projects
    Warp.run port $ monolith ini
