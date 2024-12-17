{-# LANGUAGE OverloadedStrings #-}

module HTMX where

import qualified Text.Blaze.Internal as I
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

importHTMX :: H.Html
importHTMX = H.script H.! A.src "https://unpkg.com/htmx.org@2.0.3" $ ""

hxGet :: I.AttributeValue -> I.Attribute
hxGet value = I.customAttribute "hx-get" value

hxPost :: I.AttributeValue -> I.Attribute
hxPost value = I.customAttribute "hx-post" value

hxSwap :: I.AttributeValue -> I.Attribute
hxSwap value = I.customAttribute "hx-swap" value

hxTrigger :: I.AttributeValue -> I.Attribute
hxTrigger value = I.customAttribute "hx-trigger" value

hxTarget :: I.AttributeValue -> I.Attribute
hxTarget value = I.customAttribute "hx-target" value
