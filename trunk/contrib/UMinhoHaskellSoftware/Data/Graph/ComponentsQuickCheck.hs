module Data.Graph.ComponentsQuickCheck where

import Data.Graph.Components
import Data.Relation.SetOfPairs
import qualified Data.Set as Set
import Test.QuickCheck
import Data.Relation.SetOfPairsQuickCheck ()


prop_WeakComponentsIdempotent r
  = classify (Set.size r /= 0) "non-empty" $
    classify (length once >= 2) "several components" $
    once == twice
    where
      once = weakComponents r
      twice = concatMap weakComponents once  
      types = (r::Rel Int Int)
  
prop_WeakComponentsNoLoss r
  = classify (Set.size r /= 0) "non-empty" $
    classify (length components >= 2) "several components" $
    r == foldr Set.union Set.empty components
    where
      components = weakComponents r
      types = (r::Rel Int Int)
