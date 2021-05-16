{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when, unless)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Csv (decodeByName)
import Data.Foldable (toList)
import Data.Text (unpack)
import CovidData ( CovidData) 
import StatReport ( statInfo, textReport )
import HtmlReport  (htmlReport )
import Params (cmdLineParser, Params(..) )

import Lib


generateReports :: (Functor t, Foldable t) =>
                   Params -> t CovidData -> IO ()
generateReports Params {..} covidstats = do
  unless silent $ putStr textRpt
  saveHtml htmlFile htmlRpt
  where
   statInfo' = statInfo covidstats
   textRpt = textReport statInfo'
   htmlRpt = htmlReport title covidstats statInfo' [chartFname | chart]
   
   chartFname = "chart.svg" 
   title = "Covid Data"

   saveHtml Nothing _ = pure ()
   saveHtml (Just f) html = BL.writeFile f html

work :: Params -> IO ()
work params = do
  csvData <- BL.readFile (fname params)
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, covidstats) -> generateReports params covidstats


main :: IO ()
main = cmdLineParser >>= work


readCovid :: FilePath -> IO [CovidData]
readCovid fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, covidstats) -> pure (toList covidstats)