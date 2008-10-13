module Data.Relation.Closures where

import Data.Relation.SetOfPairs
import Data.Graph.Slicing (slice)
import Data.Graph.Basics (ent)
import Data.Set as Set
-----------------------------------------------------------------------------
-- * Closures

-- | Compute the reflective closure
reflClose       :: Ord a => Rel a a -> Rel a a
reflClose xs    = xs `union` Set.map (\x -> (x,x)) (ent xs)

-- | Compute the symmetric closure
symmClose       :: Ord a => Rel a a -> Rel a a
symmClose xs    = xs `union` Set.map (\(x,y) -> (y,x)) xs
            
-- | Compute the transitive closure (naive, slow)
transClose'       :: Ord a => Rel a a -> Rel a a
transClose' xs    = if xxs == xs then xs else (transClose' xxs)
                   where 
                     xxs = union xs (comp xs xs)

-- | Compute the transitive closure (faster)
transClose       :: Ord a => Rel a a -> Rel a a
transClose xs
  = fromList $ concatMap tcFrom [ x | (x,_) <- elems xs ]
    where 
      tcFrom x = [ (x,y) | y <- elems (reachableFrom x) ]
      reachableFrom x
        = rng (slice (singleton x) xs)

-- | Compute the reflexive, transitive closure
reflTransClose    :: Ord a => Rel a a -> Rel a a
reflTransClose xs = reflClose (transClose xs)

-- | Compute the reflexive, transitive, symmetric closure, i.e. the
--   induced equivalence relation.
equiv      :: Ord b => Rel b b -> Rel b b
equiv r    = reflTransClose (r `union` (inv r))


