{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module StatReport where

import Data.Ord (comparing)
import Data.Foldable (minimumBy, maximumBy)
import Data.Time (diffDays)


import Fmt      -- used for formating text
    ( Buildable(..),
      Builder,
      (+|),
      (+||),
      pretty,
      (|+),
      (||+),
      fixedF )          
import Colonnade ( ascii, headed )


import CovidData ( field2fun, CovField, CovidData(statDate) )

-- -- dissagregate the case data to get the daily cases
-- disaggregateCases :: [CovidData] -> [Double] 
-- disaggregateCases allCases  = zipWith (-) (tail $ map cases allCases) (init $ map cases allCases)

decimalPlacesFloating :: Int
decimalPlacesFloating = 2

data StatValue = StatValue {    --format calculated fields
  decimalPlaces :: Int,
  value :: Double
}

data StatEntry = StatEntry {      --- used for storing stat results
    covfield :: CovField,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
}

computeMinMaxDays :: (Ord a, Foldable t) => 

                     (CovidData -> a) -> t CovidData -> (a,a, Int)
computeMinMaxDays get covid = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp covid
    maxQ = maximumBy cmp covid
    days = fromIntegral $ abs $ diffDays (statDate minQ) (statDate maxQ)


statInfo :: (Functor t, Foldable t) => t CovidData -> [StatEntry]
statInfo covid = fmap covFieldStatInfo [minBound .. maxBound]

  where
    decimalPlacesByCovField _ = decimalPlacesFloating

    covFieldStatInfo covfield =
      let
        get = field2fun covfield
        (mn, mx, daysBetweenMinMax) =
              computeMinMaxDays get covid
        decPlaces = decimalPlacesByCovField covfield
        minVal = StatValue decPlaces mn
        maxVal = StatValue decPlaces mx
      in StatEntry {..}


instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

instance Buildable StatEntry where
  build StatEntry {..} =
          ""+||covfield||+": "
            +|minVal|+" (min), "
            +|maxVal|+" (max), "
            +|daysBetweenMinMax|+" (days)"

textReport :: [StatEntry] -> String
textReport = ascii colStats
  where 
    colStats = mconcat
      [ headed "Covid Field" (show . covfield)
      , headed "Min" (pretty. minVal)
      , headed "Max" (pretty. maxVal)
      , headed "Days between Min/Max" (pretty . daysBetweenMinMax)
      ]

showPrice :: Double -> Builder                                          
showPrice = fixedF decimalPlacesFloating