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

import Data.Map hiding (map)

------------------------------------------------------------------------------
-- * Bags

-- | The type of bags.
newtype Bag a = Bag {fm :: Map a Int} deriving Eq

-- | Add an element to a bag.
addToBag :: Ord a => Bag a -> a -> Bag a
addToBag Bag{ fm=bag } a
  = Bag (insertWith (+) a 1 bag)

-- | Empty bag.
emptyBag :: Bag a
emptyBag = Bag empty

-- | Put all elements from a list into a bag.
listToBag :: Ord a => [a] -> Bag a
listToBag xs
  = foldr (\x bag -> addToBag bag x) emptyBag xs

-- | Create the union of a list of bags.
unionBags :: Ord a => [Bag a] -> Bag a
unionBags bags = 
  Bag $ foldr (unionWith (+)) empty $ map fm bags
  
-- | Create the union of two bags.
unionBag :: Ord a => Bag a -> Bag a -> Bag a
unionBag b1 b2 = 
  Bag $ unionWith (+) (fm b1) (fm b2)

-- | Return the frequency of the given element.
lookupBag :: Ord a => Bag a -> a -> Int 
lookupBag b a = findWithDefault 0 a (fm b) 

-- | Fold over a bag.
foldBag :: (a -> Int -> u -> u) -> u -> Bag a -> u
foldBag f u (Bag m) = foldWithKey f u m

------------------------------------------------------------------------------
