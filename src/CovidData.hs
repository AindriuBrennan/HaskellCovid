{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module CovidData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Text    (Text)
import Data.Csv (FromNamedRecord(parseNamedRecord), (.:), FromField (..))



data CovidData = CovidData {
            statDate:: Day,
            covidCases :: Double,
            icuCases :: Double,
            male :: Double,
            female :: Double,
            unknown:: Double,
            aged1to4 :: Double,
            aged5to14 :: Double,
            aged15to24 :: Double,
            aged25to34 :: Double,
            aged35to44 :: Double,
            aged45to54 :: Double,
            aged55to64 :: Double,
            aged65up :: Double
        }
    deriving (Ord, Eq, Show, Generic)

instance FromNamedRecord CovidData where
    parseNamedRecord m = 
        CovidData 
        <$> m .: "StatisticsProfileDate"
        <*> m .: "CovidCasesConfirmed"
        <*> m .: "RequiringICUCovidCases"
        <*> m .: "Male"
        <*> m .: "Female"
        <*> m .: "Unknown"
        <*> m .: "Aged1to4"
        <*> m .: "Aged5to14"
        <*> m .: "Aged15to24"
        <*> m .: "Aged25to34"
        <*> m .: "Aged35to44"
        <*> m .: "Aged45to54"
        <*> m .: "Aged55to64"
        <*> m .: "Aged65up"

instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%d/%m/%Y" . unpack
        
data CovField =  CovidCasesConfirmed | RequiringICUCovidCases | Male | Female | Unknown | Aged1to4 | Aged5to14 | Aged15to24 | Aged25to34 | Aged35to44 | Aged45to54 | Aged55to64 | Aged65up 
    deriving (Eq, Ord, Show, Enum, Bounded)

field2fun :: CovField -> CovidData -> Double
field2fun CovidCasesConfirmed = covidCases
field2fun RequiringICUCovidCases = icuCases
field2fun Male = male
field2fun Female = female
field2fun Unknown = unknown
field2fun Aged1to4 = aged1to4
field2fun Aged5to14 = aged5to14
field2fun Aged15to24 = aged15to24
field2fun Aged25to34 = aged25to34
field2fun Aged35to44 = aged35to44
field2fun Aged45to54 = aged45to54
field2fun Aged55to64 = aged55to64
field2fun Aged65up = aged65up


