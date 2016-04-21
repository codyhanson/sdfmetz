-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains data types for representing individual metrics and
-- groups of metrics. Printing support for these is supplied as well.
--
-----------------------------------------------------------------------------

module Data.Metrics where

import Data.List (transpose)
import Data.Ratio

-----------------------------------------------------------------------------
-- * Defined data structures

-- | Individual metric.
data Metric = Metric {
  description :: String, -- ^ Full description of the metric.
  name        :: String, -- ^ Small name of the metric.
  value       :: String  -- ^ Value of the calculated metric.
 }  


-- | Named group of metrics.
data GroupMetric = GroupMetric String [Metric]


-----------------------------------------------------------------------------
-- * Operations on metrics

-- | Compute summary metrics of group metrics. For each group metric, 
--   we compute minimum, average, and maximum.
summaryMetrics :: [GroupMetric] -> [Metric]
summaryMetrics gms
  = ss
  where
    ms = transpose $ map unGroupMetric gms
    ss = concatMap summary ms
    summary ms@(Metric d n v : _)
      = [ Metric d ("min"++n) (show $ minimum $ values)
        , Metric d ("avg"++n) (show $ fromRational $ average $ values)
        , Metric d ("max"++n) (show $ maximum $ values)
        ] where values = map (read.value) ms :: [Integer]
    unGroupMetric (GroupMetric _ ms) = ms

-- | Utility function to compute average of a list of integral numbers.
average :: Integral a => [a] -> Ratio a
average xs
  = (sum xs) % (fromIntegral $ length xs)

-----------------------------------------------------------------------------
-- * Functions to aid pretty-printing the metrics

-- | Creates a list of strings representing a list of 'GroupMetric's.
--   Returns the name of the group metric, an empty string, each of the
--   metrics and a final blank string.
printGroupMetricList :: [GroupMetric] -> [String]
printGroupMetricList = concatMap func
   where func (GroupMetric n ms) = [n, ""] ++ (printMetricsList (ms)) ++ [""]

-- | Creates a list of strings representing a list of 'GroupMetric's.
--   Returns only the values of the metrics (put them in a row). 
printGroupMetricRow :: [GroupMetric] -> [String]
printGroupMetricRow = printMetricsRow . concatMap getMetrics
   where getMetrics (GroupMetric _ metrics) = metrics

-- | Creates a list with the small name of the metrics (put them in a row).
printGroupMetricHeaderRow :: [GroupMetric] -> [String]
printGroupMetricHeaderRow = printMetricHeadersRow . concatMap getMetrics
   where getMetrics (GroupMetric _ metrics) = metrics

-- | Creates a string representation of a list of individual metrics. Each
--   string will contain the metric's description, the small name (between 
--   brackets and its value. The results will be aligned.
printMetricsList :: [Metric] -> [String]
printMetricsList ms
 = map (printMetric (m1,m2,m3)) ms
 where
  m1 = maximum (map (length . description) $ ms)
  m2 = maximum (map (length . name) $ ms)
  m3 = maximum (map (length . value) $ ms)
  printMetric :: (Int,Int,Int) -> Metric -> String
  printMetric (w1,w2,w3) (Metric d n v)
    = fill w1 d++" "++fill (w2+2) ("("++n++")")++" : "++fill w3 v
      where
        fill w str = take w (str++repeat ' ')

-- | Creates a list with the metrics values (put them in a row).
printMetricsRow :: [Metric] -> [String]
printMetricsRow = map (\(Metric _ _ value) -> value)

-- | Creates a list with the metrics small names (put them in a row).
printMetricHeadersRow :: [Metric] -> [String]
printMetricHeadersRow = map (\(Metric _ name _) -> name)

-----------------------------------------------------------------------------

