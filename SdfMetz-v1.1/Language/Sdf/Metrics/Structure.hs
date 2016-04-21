-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Metrics regarding the structure and complexity of Sdf grammars.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.Structure where

import Data.Metrics
import StrategyLib
import Language.Sdf.SdfLib
import Language.Sdf.FlowGraph
import Data.Relation.SetOfPairs
import Control.Monad.Identity (runIdentity)

-----------------------------------------------------------------------------
-- * Defined data structures

-- | Record which holds all structural metrics
data StructMetrics = StructMetrics
   { -- | Tree Impurity
     timp :: Float
     -- | Tree Impurity 2 (w/cycles)
   , timp2 :: Float  
     -- | Count of levels (LEV)
   , lev :: Int  
     -- | Normalized count of levels (CLEV)
   , clev :: Float
     -- | Number of non-singleton levels (NSLEV)
   , nslev :: Int
     -- | Size of largest level (DEP)
   , dep :: Int 
     -- | Maximum height of the graph (HEI)
   , hei :: Int
   }
   

-----------------------------------------------------------------------------
-- * Metric calculation functions

-- | Calculate structural metrics
calcStructMetrics :: SDF -> StructMetrics
calcStructMetrics sdf =
   let graph = calcGrammarGraph sdf
       sCM = calculateStrongComponentMetrics graph
   in StructMetrics { timp = treeImpurity graph
                    , timp2 = treeImpurityTC graph
                    , lev  = componentCount sCM
                    , clev = componentCountNormalized sCM
                    , nslev = nonSingletonComponentCount sCM
                    , dep   = componentSizeMax sCM
                    , hei   = heightOfComponentGraph sCM
                    }

-- | Creates a uniform representation of the metrics converting the internal
--   'StructMetrics' record to a 'GroupMetric' type which holds not only all the
--   calculated metric values but also all information about the metrics:
--   description and name.
structMetrics :: StructMetrics -> GroupMetric
structMetrics sm = 
   let timpM  = Metric "Tree impurity" "TIMP" (show $ timp sm)
       timpM2 = Metric "Tree impurity after trans. closure" "TIMP2" (show $ timp2 sm)
       levM   = Metric "Count of levels" "LEV" (show $ lev sm)
       clevM  = Metric "Normalized Count of Levels" "CLEV" (show $ clev sm)
       nslevM = Metric "Nr of Non-singleton Levels" "NSLEV" (show $ nslev sm)
       depM   = Metric "Size of largest levely" "DEP" (show $ dep sm)
       heiM   = Metric "Maximum height" "HEI" (show $ hei sm)
   in GroupMetric "Structural Metrics" [timpM,timpM2,levM,clevM,nslevM,depM,heiM]

-----------------------------------------------------------------------------

