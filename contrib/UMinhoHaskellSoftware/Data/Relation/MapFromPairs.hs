-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation.MapFromPairs
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of labeled relations as map from pairs to values.
--
-----------------------------------------------------------------------------

module Data.Relation.MapFromPairs where

import Data.Relation.SetOfPairs 
import qualified Data.Set as Set
import qualified Data.Map as Map

-----------------------------------------------------------------------------
-- * Representation

-- | The type of labeled relations.
type LRel a b c  = Map (a,b) c

{- Already provided by Data.FiniteMap
instance (Show a, Show b) => Show (FiniteMap a b) where
  show xs = "{" ++ show' (fmToList xs) ++  "}"
            where
             show' []        = ""
             show' (x:xs)    = showItem x ++ showl xs
             showl []        = ""
             showl (x:xs)    = "," ++ (showItem x) ++ (showl xs)
             showItem (a,b)  = show a ++ "->" ++ show b
-}

-- | Convert labeled relation to one without labels. The first argument
--   is a predicate that determines, based on the label, which pairs are
--   to be included in the result relation.
rel         :: (Ord a, Ord b) 
            => (c -> Bool)       -- ^ Predicate on labels.
            -> LRel a b c 
            -> Rel a b
rel p fm    = Set.fromList (Map.keys (Map.filter p fm))

-- | Convert relation to a labeled one. The first argument is the 
--   label with which all pairs in the result relation will be labeled.
lrel        :: (Ord a, Ord b) 
            => c                -- ^ Initialization label
            -> Rel a b 
            -> LRel a b c
lrel c r    = Map.fromList (zip (pairs r) (repeat c)) 

-- | Carrier set.
entities    :: (Ord a) => LRel a a c -> Set a
entities lr = Set.fromList ((map fst pairs) ++ (map snd pairs))
              where
                pairs = Map.keys lr  

-- | Label set.
labels      :: (Ord a, Ord b, Ord c) => LRel a b c -> Set c
labels lr   = Set.fromList $ Map.elems lr     

-----------------------------------------------------------------------------
-- * Extremal paths.

-- | Generic extremal path algorithm, based on Roland Backhouse's 
--   lecture notes, page 192 (draft version).
extremal :: Ord a 
            => b                -- ^ Zero of multiplication.
            -> (b -> b -> b)    -- ^ Multiplication. 
            -> (b -> b -> b)    -- ^ Addition.
            -> LRel a a b -> LRel a a b
extremal zero and or lr
            = foldr step lr nodes
              where
                nodes         = Set.elems (entities lr)
                step k lr     = mk [((i,j), value i j k lr) 
                                             | i <- nodes
                                             , j <- nodes 
                                   ]
                value i j k lr 
                              = (p lr (i,j)) 
                                `or` ( (p lr (i,k))
                                     `and` (p lr (k,j)) )
                mk            = Map.fromList
                p lr (i,j)    = Map.findWithDefault zero (i,j) lr

-- | Reachability. This is transitive closure following Roy-Warshall.
reach       :: Ord a => LRel a a Bool -> LRel a a Bool
reach r     = extremal False (&&) (||) r

-- | Least cost, or shortest path. 
leastcost       :: (Num cost, Ord cost, Ord a) 
                => cost               -- ^ Maximum bound. 
                -> LRel a a cost      -- ^ Cost labels must be non-negative.
                -> LRel a a cost
leastcost maxBound r 
                = extremal maxBound (+) min r

-- | Worst cost, or longest path. UNTESTED.
worstcost       :: (Ord a, Num cost, Ord cost) 
                => cost
                -> LRel a a cost
                -> LRel a a cost
worstcost maxBound r   
                = extremal maxBound (+) max r

-- | Bottle neck.
bottleneck      :: (Ord height, Num height, Ord a) 
                => LRel a a height      -- ^ Height labels must be non-negative.
                -> LRel a a height
bottleneck r    = extremal 0 min max r

-- | Least cost, or shortest path. DOES NOT WORK, because it relies on 
--   maxBound from the Bounded class, leading to erroneous arithmetic.
leastcost'      :: (Num cost, Ord cost, Bounded cost, Ord a) 
                => LRel a a cost -> LRel a a cost
leastcost' r    = extremal maxBound (+) min r

-- | Not quite right when cycles are present.
height :: Ord a => Rel a a -> Integer
height g = Map.fold (\a b -> maybe b (max b) a) 0 $ heights 
  where
    heights = extremal zero and or lr
    lr = lrel (Just 2) g
    zero = Nothing
    and (Just x) (Just y) = Just (x+y-1)
    and _ _ = Nothing
    or (Just x) (Just y) = Just $ max x y
    or (Just x) Nothing  = Just $ x
    or Nothing (Just y)  = Just $ y
    or _ _               = Nothing
    
    

