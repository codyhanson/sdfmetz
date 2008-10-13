-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Joost Visser, Alexandra Silva
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains functionality for dividing graphs into components,
-- including strong connected component analysis.
--
-----------------------------------------------------------------------------

module Data.Graph.Components where

import qualified Data.Set as Set 
import Data.Graph.Slicing
import Data.Graph.Basics
import Data.Map as Map

-----------------------------------------------------------------------------
-- * Weak components

-- | Compute subrelation connected to seeds, stopping at stoppers.
gobbleUntil        :: Ord a => Set a -> Set a -> Gph a -> Gph a
gobbleUntil seed stop r    
  = if (Set.null s) 
      then emptyRel 
      else r' `Set.union` (gobbleUntil seed' stop' r)
    where
      s = seed `Set.difference` stop                         -- live seeds
      r' = (project s r) `Set.union` (projectBackward s r)   -- step
      seed' = ent r'                                         -- new seeds
      stop' = stop `Set.union` s                             -- extend stoppers

-- | Compute weakly connected components.
weakComponents :: Ord a => Gph a -> [Gph a]
weakComponents r
  = weakComponentsUntil Set.empty r
    where
      weakComponentsUntil done r
        = case Set.elems ((ent r) `Set.difference` done) of
           []    -> []
           (a:_) -> (r':s)
                    where
                      r' = gobbleUntil (Set.singleton a) Set.empty r
                      s  = weakComponentsUntil (done `Set.union` (ent r')) r   

-- | Compute weakly connected components (sets of nodes)
weakComponentSet :: Ord a => Gph a -> Set (Set a)
weakComponentSet r
 = Set.fromList (Prelude.map ent $ weakComponents r)

-----------------------------------------------------------------------------
-- * Strong components

-- ** Components as sets of nodes

-- | Compute a single strong component (set of nodes).
strongComponent :: (Ord a) => a -> Gph a -> Set a
strongComponent x r
  = Set.insert x reach
    where
      reach = (reachableFrom x r) `Set.intersection` (reachableFrom x (inv r))
      reachableFrom x r
        = ent (slice (Set.singleton x) r)
        
-- | Compute a single strong component (set of nodes).
strongComponentExcluding :: (Ord a) => Set a -> a -> Gph a -> Set a
strongComponentExcluding done x r
  = Set.insert x reach
    where
      reach = (reachableFrom x r) `Set.intersection` (reachableFrom x (inv r))
      reachableFrom x r
        = ent (sliceUntil (Set.singleton x) done r)
        
-- | Compute the set of strong components (sets of nodes).
--   Not optimized.
strongComponentSet :: (Ord a, Ord (Set a)) => Gph a -> Set (Set a)
strongComponentSet r
  = Set.map (\x -> strongComponent x r) (ent r)
  
{-
-- | Compute the graph of strong components (node sets).
strongComponentGraph :: (Ord a, Ord (Set a)) => Gph a -> Graph (Set a) 
strongComponentGraph r
  = (st, removeSelfEdges gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = findWithDefault (error "SCC map misses entry") x mp
      gph = Set.map edgeT r
      st = Set.fromList $ elems mp
      -- Using finite map from nodes to SSCs for performance reasons:
      mp = fromList $ Prelude.map (\x -> (x, strongComponent x r))  $ Set.elems $ ent r
-}

-- | Compute the graph of strong components (node sets).
strongComponentGraph :: (Ord a, Ord (Set a)) => Gph a -> Graph (Set a) 
-- strongComponentGraph r = componentGraph strongComponent r
strongComponentGraph r = componentGraph' strongComponentExcluding id r

componentGraph 
  :: (Ord node, Ord comp)
  => (node -> Gph node -> comp) 
  -> Gph node 
  -> Graph comp
componentGraph mkComponent r
  = (st,gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = findWithDefault (error "SCC map misses entry") x mp
      gph = Set.fold worker Set.empty r
      worker (x,y) g = insertNonSelfEdge (nodeT x) (nodeT y) g
      insertNonSelfEdge xc yc g = if xc==yc then g else Set.insert (xc,yc) g
      st = Set.fromList $ elems mp
      -- Using finite map from nodes to components for performance reasons:
      mp = Set.fold (\x -> Map.insert x (mkComponent x r)) Map.empty (ent r)

componentGraph' 
  :: (Ord node, Ord comp)
  => (Set node -> node -> Gph node -> comp) 
  -> (comp -> Set node)
  -> Gph node 
  -> Graph comp
componentGraph' mkComponent getEnt r
  = (st,gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = findWithDefault (error "SCC map misses entry") x mp
      gph = Set.fold worker Set.empty r
      worker (x,y) g = insertNonSelfEdge (nodeT x) (nodeT y) g
      insertNonSelfEdge xc yc g = if xc==yc then g else Set.insert (xc,yc) g
      st = Set.fromList $ elems mp
      -- Using finite map from nodes to components for performance reasons:
      (mp,_) = Set.fold addCmp (Map.empty,Set.empty) (ent r)
      addCmp x (mp,done) 
        | x `Set.member` done = (mp,done)
        | otherwise           = (mp',done')
        where
          cmp = mkComponent done x r
          entcmp = getEnt cmp
          mp' = Set.fold (\x -> Map.insert x cmp) mp entcmp
          done' = done `Set.union` entcmp

-- | Compute the set of strong components that have more than a single node
strongNonSingletonComponentSet :: (Ord a, Ord (Set a)) 
                               => Gph a -> Set (Set a)
strongNonSingletonComponentSet
  = Set.filter (\c -> Set.size c > 1) . strongComponentSet

-- ** Components as graphs

-- | Type of components: node set tupled with subgraph.
type Component a = Graph a

-- | Compute a single strong component (node set tupled with subrelation).
strongComponent' :: (Ord a) => a -> Rel a a -> Component a
strongComponent' x r
  = (scc,subrel)
    where
      scc = strongComponent x r
      subrel = Set.filter (\(x,y) ->  x `Set.member` scc && y `Set.member` scc ) r

strongComponentExcluding' :: (Ord a) => Set a -> a -> Rel a a -> Component a
strongComponentExcluding' done x r
  = (scc,subrel)
    where
      scc = strongComponentExcluding done x r
      subrel = Set.filter (\(x,y) ->  x `Set.member` scc && y `Set.member` scc ) r

-- | Compute the graph of stong components (node sets tupled with subrelations).
strongComponentRel' :: (Ord a, Ord (Set a)) 
                    => Gph a -> Gph (Component a)
-- strongComponentRel' r = snd $ componentGraph strongComponent' r
strongComponentRel' r = snd $ componentGraph' strongComponentExcluding' fst r
{-
  = removeSelfEdges $ Set.map edgeT r
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = strongComponent' x r
-}

{- Inefficient, use (snd $ strongComponentGraph) instead.
-- | Compute the graph of strong components (node sets).
--   Note: strong components that have no relations to other components
--   will not be present in the graph!
strongComponentRel :: (Ord a, Ord (Set a)) => Gph a -> Gph (Set a) 
strongComponentRel r
  = removeSelfEdges $ Set.map edgeT r
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = (strongComponent x r)
-}

-----------------------------------------------------------------------------
-- * Othter graph partitionings

-- | Derive a graph of groups, based on a partitioning of the
--   nodes into groups.
partitionGraph :: Ord a => Set (Set a) -> Gph a -> Gph (Set a)
partitionGraph groups g
  = removeSelfEdges $ Set.map edgeT g
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = findWithDefault (error "Group map misses entry") x mp
      mp = Set.fold worker Map.empty $ groups
      worker xs mp = Set.fold (\x mp -> Map.insert x xs mp) mp xs

-- | Split a graph into subgraphs, stored in a map with groups as keys.
subGraphs 
  :: Ord a 
  => Set (Set a)           -- ^ groups
  -> Gph a                 -- ^ graph
  -> Map (Set a) (Gph a)   -- ^ map from groups to corresponding subgraphs
subGraphs groups g = Set.fold worker Map.empty groups
  where
    worker group gs = Map.insert group (subGraph group g) gs

-- | Determine the subgraph of a given graph induced by a set of nodes.
subGraph :: Ord a => Set a -> Gph a -> Gph a
subGraph xs g = Set.fold worker emptyRel g
  where
    worker e@(x,y) g 
      | x `Set.member` xs && y `Set.member` xs = Set.insert e g
      | otherwise = g
 
-----------------------------------------------------------------------------
