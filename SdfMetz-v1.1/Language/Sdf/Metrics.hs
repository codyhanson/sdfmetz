-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Metrics for Sdf grammars.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics where

import Data.Metrics
import Language.Sdf.Metrics.Ambiguity
import Language.Sdf.Metrics.Structure
import Language.Sdf.Metrics.Size
import Language.Sdf.Metrics.Halstead
import Language.Sdf.SdfLib

-----------------------------------------------------------------------------

-- | Calculates a list of group metrics from an SDF grammar.
--   Currently, four groups are produced, for size metrics,
--   Halstead metrics, structure metrics, and ambibuity metrics.
calculateMetrics :: SDF -> [GroupMetric]
calculateMetrics sdf = 
   let sizemetrics      = (sizeMetrics   . calcSizeMetrics)   sdf
       halmetrics       = (halsteadMetrics . calcHalsteadMetrics) sdf
       structmetrics    = (structMetrics . calcStructMetrics) sdf
       ambiguitymetrics = (ambigMetrics  . calcAmbigMetrics)  sdf
   in [sizemetrics, halmetrics, structmetrics, ambiguitymetrics]

-----------------------------------------------------------------------------
