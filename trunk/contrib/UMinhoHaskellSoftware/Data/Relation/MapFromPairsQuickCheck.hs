-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation.MapFromPairsQuickCheck
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Properties of the implementation of labeled relations as maps from pairs.
--
-----------------------------------------------------------------------------

module Data.Relation.MapFromPairsQuickCheck where

import Data.Set
import qualified Data.Map as Map
import Data.Relation.SetOfPairs
import Data.Relation.MapFromPairs
import Data.Relation.Closures (transClose)
import Data.Relation.SetOfPairsQuickCheck () -- only instances
import Test.QuickCheck

-----------------------------------------------------------------------------

instance (Ord a, Arbitrary a, Arbitrary b)
      => Arbitrary (Map a b) where
  arbitrary = do xs <- arbitrary
                 return (Map.fromList xs)
                 
-----------------------------------------------------------------------------

prop_TransCloseReach r
  = classify (size r /= 0) "non-empty" $
    rel id (reach (lrel True r)) == transClose r
    where
      types = (r::Rel Int Int)

-----------------------------------------------------------------------------

