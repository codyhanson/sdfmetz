
module Data.Graph.Basics
(
module Data.Graph.Basics,
module Data.Relation.SetOfPairs
)
where

import qualified Data.Set as Set
import Data.Relation.SetOfPairs
import Data.Relation.MapFromPairs


-- | Type of graphs: relations with domain and range of the same type.
type Gph a = Rel a a       

-- | Type of graphs, with explicit entity set.
type Graph a = (Set a, Gph a)

-----------------------------------------------------------------------------
-- * Reductions

-- | Reflexive reduction, i.e remove self-edges.
reflReduc :: Ord a => Gph a -> Gph a
--reflReduc g = mkRel $ filter (\(x,y) -> x /= y) $  Set.elems g
reflReduc g = Set.filter (\(x,y) -> x /= y) g

-----------------------------------------------------------------------------
-- * Basic graph queries

-- | Find top nodes of a graph. Those nodes that have outgoing but no
--   incoming edges. 
topNodes :: Ord a => Gph a -> Set a
topNodes g = dom g `Set.difference` rng g

-- | Find bottom nodes of a graph. Those nodes that have incoming but no
--   outgoing edges. 
bottomNodes :: Ord a => Gph a -> Set a
bottomNodes g = rng g `Set.difference` dom g

-- | Find internal nodes of a graph. Those nodes that have both 
--   incoming and outgoing edges. 
internalNodes :: Ord a => Gph a -> Set a
internalNodes g = rng g `Set.intersection` dom g

-- | Remove all edges of the form (x,x).
removeSelfEdges :: Ord a => Gph a -> Gph a
removeSelfEdges r
  = mkRel [ (x,y) | (x,y) <- Set.elems r, x /= y ]

-- | Obtain all entities in a relation (union of domain and range)
ent             :: Ord a => Rel a a -> Set a
ent xs          = dom xs `Set.union` rng xs


-- | Obtain the subrange and subdomain of a relation given a subdomain.
entWith             :: Ord a => Set a -> Rel a a -> Set a
entWith as r        = domWith as r `Set.union` rngWith as r

-----------------------------------------------------------------------------
-- * Costs with infinity

-- | Compute depth of a graph.
depth           :: (Ord a) => Gph a -> Int
depth r         = foldr (\d a -> maybe a (max a) d) 0 $ 
                   Set.elems $ labels $ 
                    extremal infDepth addDepth maxDepth $ 
                      lrel (mkDepth (1::Int)) r
  where
   -- We model depth as (Maybe Int), where Nothing denotes
   -- infinite cost.
   infDepth = Nothing
   mkDepth n = Just n
   maxDepth Nothing d = d
   maxDepth d Nothing = d
   maxDepth (Just d) (Just d') = Just (max d d')
   addDepth Nothing d = Nothing
   addDepth d Nothing = Nothing
   addDepth (Just d) (Just d') = Just (d + d')

-----------------------------------------------------------------------------
