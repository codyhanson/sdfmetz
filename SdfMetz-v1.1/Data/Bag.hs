-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Joost Visser
-- Stability   :  experimental
-- Portability :  experimental
--
-- Data type of bags.
-- 
-----------------------------------------------------------------------------

module Data.Bag where

import Data.FiniteMap

------------------------------------------------------------------------------
-- * Bags

-- | The type of bags.
newtype Bag a = Bag {fm :: FiniteMap a Int} deriving Eq

-- | Add an element to a bag.
addToBag :: Ord a => Bag a -> a -> Bag a
addToBag Bag{ fm=bag } a
  = Bag (addToFM_C (+) bag a 1)

-- | Empty bag.
emptyBag :: Bag a
emptyBag = Bag emptyFM

-- | Put all elements from a list into a bag.
listToBag :: Ord a => [a] -> Bag a
listToBag xs
  = foldr (\x bag -> addToBag bag x) emptyBag xs

-- | Create the union of a list of bags.
unionBags :: Ord a => [Bag a] -> Bag a
unionBags bags = 
  Bag $ foldr (plusFM_C (+)) emptyFM $ map fm bags
  
-- | Create the union of two bags.
unionBag :: Ord a => Bag a -> Bag a -> Bag a
unionBag b1 b2 = 
  Bag $ plusFM_C (+) (fm b1) (fm b2)

-- | 
lookupBag :: Ord a => Bag a -> a -> Int 
lookupBag b a = lookupWithDefaultFM (fm b) 0 a

------------------------------------------------------------------------------
