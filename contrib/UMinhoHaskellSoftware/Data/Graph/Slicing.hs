module Data.Graph.Slicing where

import Data.Relation.SetOfPairs 
import Data.Set 

-----------------------------------------------------------------------------
-- * Slicing and chopping

-- | Compute forward slice starting from seeds.
slice            :: Ord a => Set a -> Rel a a -> Rel a a
slice seed r        = sliceUntil seed empty r

-- | Compute forward slice starting from seeds, stopping at stoppers.
sliceUntil        :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
sliceUntil seed stop r    
                = if (Data.Set.null s) then emptyRel 
                                    else r' `union`
                                         (sliceUntil seed' stop' r)
              where
                s = seed `difference` stop    -- live seeds
                r' = project s r            -- step
                seed' = rng r'              -- new seeds
                stop' = stop `union` s      -- extend stoppers

-- | Compute backward slice starting from seeds.
sliceBackward        :: Ord a => Set a -> Rel a a -> Rel a a
sliceBackward seed r    = inv (slice seed (inv r))

-- | Compute chop between seeds and sinks.
chop                    :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
chop seeds sinks r  = slicesWith intersection seeds sinks r
                      
-- | Compute union of backward and forward slice.
slicesUnion :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
slicesUnion seeds sinks r
  = (slice seeds r) `union` (sliceBackward sinks r)

-- | Compute intersection of backward and forward slice.
--   This is the same as the computing the chop between seeds and sinks.
slicesIntersect :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
slicesIntersect seeds sinks r  
  = (slice seeds r) `intersection` (sliceBackward sinks r)
  
-- | Compute combination of backward and forward slice, where
--   a binary operator is supplied to specify the kind of combination.
slicesWith :: Ord a 
           => (Rel a a -> Rel a a -> Rel a a) 
           -> Set a -> Set a -> Rel a a -> Rel a a
slicesWith binop seeds sinks r
  = (slice seeds r) `binop` (sliceBackward sinks r)

-- | Compute slice or chop, depending on whether the source or sink set or both 
--   are empty.
sliceOrChopWith :: (Ord a) 
                => (Rel a a -> Rel a a -> Rel a a)  -- ^ binary graph operation
                -> [a]                              -- ^ sources
                -> [a]                              -- ^ sinks
                -> Rel a a                          -- ^ input relation
                -> Rel a a                          -- ^ output relation
sliceOrChopWith binop [] [] rel         = rel
sliceOrChopWith binop [] sinks rel      = sliceBackward (fromList sinks) rel
sliceOrChopWith binop sources [] rel    = slice (fromList sources) rel
sliceOrChopWith binop sources sinks rel 
  = slicesWith binop (fromList sources) (fromList sinks) rel
      
-----------------------------------------------------------------------------



