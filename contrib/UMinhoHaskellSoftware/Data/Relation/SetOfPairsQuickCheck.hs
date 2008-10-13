-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation.SetOfPairsQuickCheck
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Properties of the implementation of relations as sets of pairs.
--
-----------------------------------------------------------------------------

module Data.Relation.SetOfPairsQuickCheck where

import Data.Set
import Data.Relation.SetOfPairs()
import Data.Graph.Basics
import Test.QuickCheck

-----------------------------------------------------------------------------

instance (Ord a, Arbitrary a)
      => Arbitrary (Set a) where
  arbitrary = do xs <- arbitrary
                 return (fromList xs)

-- | Inverting twice is identity.
prop_InvInv r	
  = classify (size r == 0) "empty" $
    inv (inv r) == r
    where 
      types = r::Rel Int Integer

-- | Domain of inverse is range.
prop_DomInvRng :: Rel Int Integer -> Property
prop_DomInvRng r
  = classify (size r == 0) "empty" $
    dom (inv r) == rng r    
    where 
      types = r::Rel Int Integer

-- | Range of inverse is domain.
prop_RngInvDom r
  = classify (size r == 0) "empty" $
    rng (inv r) == dom r    
    where 
      types = r::Rel Int Integer

-- | Carrier of inverse is cariere of original.
prop_EntInvEnt r
  = classify (size r == 0) "empty" $
    ent (inv r) == ent r    
    where 
      types = r::Rel Int Int

-- | Inverse distributes over composition.
prop_CompInvDist r1 r2
  = classify (size r1 == 0 && size r2 == 0) "both empty" $
    classify (size r2 /= 0 && size r2 /= 0) "none empty" $
    inv (r2 `comp` r1) == (inv r1) `comp` (inv r2)
    where 
      types = (r1::Rel Int Integer, r2::Rel Integer Double)



