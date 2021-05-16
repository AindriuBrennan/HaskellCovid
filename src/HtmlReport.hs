{-# LANGUAGE OverloadedStrings #-}

module HtmlReport where

import Data.Foldable (traverse_)
import Control.Monad (unless)
import Data.ByteString.Lazy (ByteString)
import Text.Blaze.Html5 as H
    ( Html,
      string,
      text,
      ToValue(toValue),
      (!),
      body,
      docTypeHtml,
      h1,
      head,
      i,
      img,
      style,
      title )
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Colonnade ( headed, Colonnade, Headed )
import Text.Blaze.Colonnade ( encodeHtmlTable )
import Fmt (pretty, Buildable)

import CovidData (CovidData(statDate, covidCases, icuCases, male, female, aged1to4, aged5to14,aged15to24, aged25to34, aged35to44, aged45to54, aged55to64, aged65up))
import StatReport ( StatEntry(covfield, minVal, maxVal, daysBetweenMinMax))

viaFmt :: Buildable a => a -> Html
viaFmt = text . pretty


colStats :: Colonnade Headed StatEntry Html
colStats = mconcat
    [ headed "Covid Field" (i . string . show . covfield)
    , headed "Min" (viaFmt . minVal)
    , headed "Max" (viaFmt . maxVal)
    , headed "Days between Min/Max" (viaFmt . daysBetweenMinMax)
    ]

htmlReport :: (Functor t, Foldable t) =>
              String -> t CovidData -> [StatEntry] -> [FilePath] -> ByteString
htmlReport docTitle covidstats statEntries images = renderHtml $ docTypeHtml $ do 
     H.head $ do
       title $ string docTitle
       style tableStyle
     body $ do
       unless(null images) $ do
         h1 "Charts"
         traverse_((img!).src.toValue) images
        
       h1 "Statistics Report"
       encodeHtmlTable mempty colStats statEntries
  where
    tableStyle = "table {border-collapse: collapse}" <>
            "td, th {border: 1px solid black; padding: 5px}"