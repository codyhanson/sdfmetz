-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Metrics regarding the ambibuity constructs of Sdf.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.Ambiguity where

import Data.Metrics
import StrategyLib
import Language.Sdf.SdfLib
import Data.Set as Set hiding (map, filter)
import Control.Monad.Identity (runIdentity)

-----------------------------------------------------------------------------
-- * Defined data structures

-- | Data type to hold all metric results
data AmbigMetrics = AmbigMetrics
   {  -- | Number of follow restrictions
     frst :: Int
     -- | Number of associativity attributes in context-free productions
   , assoc :: Int
     -- | Number of reject productions
   , rejP :: Int
     -- | Number of unique productions in priorities
   , uppri :: Float  
     -- | Total number of preference attributes
   , pref :: Int
   }


-----------------------------------------------------------------------------
-- * Functions to calculate and print all metrics

-- | Calculates all ambiguity-related metrics.
calcAmbigMetrics :: SDF -> AmbigMetrics
calcAmbigMetrics sdf =
   let prods = (filter (not . isReject) . collectLexAndCfProductions) sdf
       priorts = (collectLexPriorities sdf)++(collectCfPriorities sdf)
   in AmbigMetrics { frst    = calcFollowRestr sdf
                   , assoc   = calcAssocAttr prods
                   , rejP    = calcRejectProds sdf
                   , uppri   = calcUPPri priorts
                   , pref    = calcPrefAttr prods
                   }

-- | Creates a uniform representation of the metrics converting the internal
--   'AmbigMetrics' record to a 'GroupMetric' type which holds not only all the
--   calculated metric values but also all information about the metrics:
--   description and name.
ambigMetrics :: AmbigMetrics -> GroupMetric
ambigMetrics am =
   let frstM   = Metric "Nr of follow restrictions" "FRST" (show $ frst am)
       assocM  = Metric "Nr of associativity attributes" "ASSOC" (show $ assoc am)
       rejpM   = Metric "Nr of reject production" "REJP" (show $ rejP am)
       uppriM  = Metric "Nr of unique prods in priorities" "UPP" (show $ uppri am)
       prefM  = Metric "Nr of preference attributes" "PREF" (show $ pref am)
       metrics = [frstM, assocM, rejpM, uppriM, prefM]
   in GroupMetric "Ambiguity-related Metrics" metrics


-----------------------------------------------------------------------------
-- * Individual metric calculation functions

-- | Calculate the number of follow restrictions
calcFollowRestr :: SDF -> Int
calcFollowRestr = runIdentity . applyTU strategy 
   where strategy = full_tdTU $ (constTU 0) `adhocTU` countFollowRestr
         countFollowRestr (Sdf_follow _ _) = return 1


-- | Calculate the number of assoc attributes in productions
calcAssocAttr :: [Production] -> Int
calcAssocAttr = runIdentity . applyTU strategy
   where strategy = full_tdTU $ (constTU 0) `adhocTU` (\(x :: Associativity) -> return 1)

-- | Calculate the number of prefer and ooid attributes in productions
calcPrefAttr :: [Production] -> Int
calcPrefAttr = runIdentity . applyTU strategy
   where 
     strategy = full_tdTU $ (constTU 0) `adhocTU` action
     action (Sdf_avoid) = return 1
     action (Sdf_prefer) = return 1
     action _ = return 0


-- | Calculate the number of reject productions (also lexical).
calcRejectProds :: SDF -> Int
calcRejectProds = length . filter isReject . collectLexAndCfProductions

-- | Calculate the number of unique productions in priorities
--   NOTE: this function have a very peculiar behavior without (map show),
--         because it creates a set with a single element. Doing (map show)
--         the cardinality of the set have correct value.
calcUPPri :: [Priority] -> Float
calcUPPri = fromIntegral . nrUniqueElems . map show . collectProductions


-- | Calculate total number of unique productions in each group
calcTNUPEG :: [Priority] -> Float
calcTNUPEG = fromIntegral . sum . map (uniqueProdCnt) . collectGroups
   where uniqueProdCnt :: Group -> Int
         uniqueProdCnt = nrUniqueElems . map show . collectProductions


-- | Calculate the total number of priority chains
calcTNPC :: [Priority] -> Float
calcTNPC = fromIntegral . length


-- | Calculate the total number of groups
calcTNG :: [Priority] -> Float
calcTNG = fromIntegral . length . collectGroups


-----------------------------------------------------------------------------
-- * Auxiliary functions

-- | Collect productions inside priorities.
collectPriorityProductions :: [Priority] -> [Production]
collectPriorityProductions
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where worker (p::Production) = return [p]


-- | Collect groups inside priorities. A group can have a single or multiple 
--   productions
collectGroups :: [Priority] -> [Group]
collectGroups = runIdentity . (applyTU $ full_tdTU strategy)
   where strategy = (constTU []) `adhocTU` (\(x::Group) -> return [x])


-- | Helper function to calculate the number of unique elements of a list
nrUniqueElems :: (Ord a) => [a] -> Int
nrUniqueElems = size . fromList

-----------------------------------------------------------------------------
