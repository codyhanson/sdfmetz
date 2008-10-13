-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Joost Visser, Tiago Alves
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains functionality various graph-based structure metrics,
-- including tree impurity, coupling, and strong component metrics.
--
-----------------------------------------------------------------------------

module Data.Graph.Metrics where

import qualified Data.Map as Map
import qualified Data.Set as Set 
import Data.Graph.Components
import Data.Relation
import Data.Graph.Basics
import Data.Metrics (asPercentageOf)


-----------------------------------------------------------------------------
-- * Structure metrics

--------------------------------------
-- ** Tree impurity

-- | Tree impurity (TIMP): 
--   how much does the graph resemble a tree, expressed as
--   a percentage. A tree has tree impurity of 0 percent. A fully connected
--   graph has tree impurity of 100 percent. Fenton and Pfleeger only
--   defined tree impurity for non-directed graphs without self-edges. This
--   implementation therefore does not count self-edges, and counts 2 directed
--   edges between the same nodes only once.
treeImpurity :: Ord a => Gph a -> Float
treeImpurity g 
  | n==2 || n==1 || n==0
  -- In these cases, the max number of edges more than the spanning tree
  -- is 0, so the tree impurity should also be 0.
  = 0
  | otherwise
  = (2 * (e - n + 1)) / ((n - 1) * (n - 2)) * 100
  where e = (realToFrac $ Set.size $ reflReduc $ symmClose g) / 2
        n = realToFrac $ Set.size $ ent g

-- | Tree impurity after transitive closure:
-- This metric differs computes tree impurity not on the graph
--   itself, but on its transitive closure.
treeImpurityTC :: Ord a => Gph a -> Float
treeImpurityTC g = treeImpurity $ transClose g

--------------------------------------
-- ** Component metrics

-- | A record type to hold metrics about strong components.
data StrongComponentMetrics = StrongComponentMetrics {
   -- | Count of strong components
   componentCount :: Int,
   -- | Normalized count of strong components
   componentCountNormalized :: Float,
   -- | Count of non-singleton components
   nonSingletonComponentCount :: Int,
   -- | Size of largest component
   componentSizeMax :: Int,
   -- | Height of component DAG
   heightOfComponentGraph :: Int
 }

-- | Calculate the strong component metrics for a given graph.
calculateStrongComponentMetrics :: Ord a => Gph a -> StrongComponentMetrics
calculateStrongComponentMetrics g
  = calculateStrongComponentMetrics' (ent g) (strongComponentGraph g)

-- | Calculate the strong component metrics for a given component graph.
calculateStrongComponentMetrics' 
  :: Ord a 
  => Set a   -- ^ Nodes of the underlying graph, need for normalized component count.
  -> Graph (Set a) 
  -> StrongComponentMetrics
calculateStrongComponentMetrics' nodes sCG 
  = calculateComponentMetrics heightDag nodes sCG

-- | Calculate the strong component metrics for a given component graph.
calculateComponentMetrics 
  :: Ord a 
  => (Gph (Set a) -> Int)   -- ^ Appropriate height function
  -> Set a   -- ^ Nodes of the underlying graph, needed for normalized component count.
  -> Graph (Set a) 
  -> StrongComponentMetrics
calculateComponentMetrics height nodes sCG = StrongComponentMetrics {
   componentCount = l,
   componentCountNormalized = l `asPercentageOf` n,
   nonSingletonComponentCount = Set.size sNSCS,
   componentSizeMax = maximum $ (0:) $ map Set.size $ Set.elems sCS,
   heightOfComponentGraph = height sCR
 } where
    sCS = fst sCG
    sCR = snd sCG
    sNSCS = Set.filter (\c -> Set.size c > 1) sCS
    l = Set.size sCS
    n = Set.size $ nodes

-- | Count of levels (LEV):
countOfLevels :: Ord a => Gph a -> Int
countOfLevels 
  = componentCount . calculateStrongComponentMetrics

-- | Normalized count of levels (CLEV):
normalizedCountOfLevels :: Ord a => Gph a -> Float
normalizedCountOfLevels 
  = componentCountNormalized . calculateStrongComponentMetrics

-- | Number of non-singleton levels (NSLEV)
numberOfNonSingletonLevels :: Ord a => Gph a -> Int
numberOfNonSingletonLevels
  = nonSingletonComponentCount . calculateStrongComponentMetrics

-- | Size of largest level (DEP):
sizeOfLargestLevel :: Ord a => Gph a -> Int
sizeOfLargestLevel
  = componentSizeMax . calculateStrongComponentMetrics
  
-- | Height for acyclic graphs (does not work correctly in the presence of cycles).
heightDag :: Ord a => Gph a -> Int
heightDag g = heightFrom g Set.empty (topNodes g)

-- | Compute graph height (length of longest path in terms of number of nodes),
--   starting from a given set of nodes.
heightFrom :: Ord a 
           => Gph a        -- ^ Graph
           -> Set a        -- ^ Already visited nodes
           -> Set a        -- ^ Nodes still to visit
           -> Int          -- ^ Number of nodes on the longest path
heightFrom g visited tovisit 
  = maximum $ (0:) $ map (+1) $ Set.elems heights
  where
    currentNodes = tovisit `Set.difference` visited
    heights = Set.map heightFrom1 currentNodes
    heightFrom1 n = heightFrom g (Set.insert n visited) (rngWith (Set.singleton n) g)

-- | Compute the longest path through a graph.
height       :: Ord a => Gph a -> Int
height g
  = Set.fold (\(x,_) n -> n `max` longestFrom x) 0 g
    where 
      longestFrom x 
        = heightUntil (Set.singleton x) Set.empty (outEdgesMap g)

-- | Compute longest path starting from seeds, stopping at stoppers.
heightUntil 
  :: Ord a 
  => Set a        -- ^ Nodes still to visit (seeds) 
  -> Set a        -- ^ Already visited nodes (stoppers) 
  -> Map a [a]    -- ^ Map of nodes to their immediate kids
  -> Int          -- ^ Number of nodes on the longest path
heightUntil seed stop childMap
  = if (Set.null s)
      then 0 
      else 1 + (heightUntil seed' stop' childMap)
    where
      s     = seed Set.\\ stop
      cm'   = Map.filterWithKey (\x _ -> x `Set.member` s) childMap 
      seed' = Map.fold (\ys s' -> s' `Set.union` (Set.fromList ys)) Set.empty cm' 
      stop' = stop `Set.union` s      

-- | Helper function that transforms a graph into a map
--   from nodes to their child lists. Used for optimization.
outEdgesMap :: Ord a => Gph a -> Map a [a]
outEdgesMap g
  = Set.fold worker Map.empty g
    where
      worker (x,y) mp 
        = Map.insertWith (\_ ys -> (y:ys)) x [y] mp
        
-- | Height (works also in the presence of cycles, but very slow).
heightSlow :: Ord a => Gph a -> Int
heightSlow g 
  = (heightFrom newG Set.empty (Set.singleton Nothing)) - 1
  where
    justG = Set.map (\(x,y)->(Just x, Just y)) g
    newEdges = Set.map (\x -> (Nothing, x)) (dom justG)
    newG = newEdges `Set.union` justG 

-----------------------------------------------------------------------------
-- ** Fan in and out

-- | Compute fan-in and fan-out of a given graph. Both are represented
--   with a bags of nodes. The arity of each node in the bag is its
--   fanout or fanin, repectively.
fanInOut :: Ord a => Gph a -> (Bag a, Bag a)
fanInOut g = foldr fan1 (emptyBag,emptyBag) $ Set.elems g
  where
    fan1 (a,b) (fanIn,fanOut)
      = (addToBag fanIn b, addToBag fanOut a)
      
-----------------------------------------------------------------------------
-- ** Coupling and coherence

-- | Given a partition of the domain into components (sets of nodes),
--   compute for each component the number of outgoing edges, incoming
--   edges, internal edges, and instability. 
couplings 
  :: (Ord a, Integral n)
  => Set (Set a)                       -- ^ groups
  -> Gph a                             -- ^ graph
  -> Map (Set a) (n,n,n,Float,Float)   -- ^ map from groups to coupling metrics
couplings aas g = Set.fold worker Map.empty aas
  where
    worker as mp = Map.insert as (coupling as g) mp

coupling :: (Ord a, Integral n)
          => Set a -> Gph a -> (n,n,n,Float,Float)
coupling as g = (ce,ca,ci,instability,coherence)
  where
    (ce,ca,ci) = Set.fold worker (0,0,0) g
    instability = ce `asPercentageOf` (ca+ce)
    coherence   = if Set.size as /= 1
                     then ci `asPercentageOf` (ca+ce+ci)
                     else 100   -- Singleton groups have 100% coherence.
    worker (x,y) (ce,ca,ci) 
      = case (x `Set.member` as,y `Set.member` as) of
          (True,True) -> (ce,ca,ci+1)
          (True,_)    -> (ce+1,ca,ci)
          (_,True)    -> (ce,ca+1,ci)
          otherwise   -> (ce,ca,ci)
          
-----------------------------------------------------------------------------

{-
-- * Height again ... old approaches

height' :: Ord a => Gph a -> Int
height' g 
  = (heightFrom g Set.empty startNodes)
  where
    (scs,scg) = strongComponentGraph g
    topComponents = (topNodes scg) `Set.union` (scs Set.\\ ent scg)
    startNodes = Set.fold worker Set.empty topComponents
    worker cmpnt ns = Set.insert (pick_one $ Set.toList cmpnt) ns
    pick_one []    = error "Component should not be empty!!"
    pick_one (x:_) = x      

-- | Compute a set of nodes from which all other nodes can be reached.
startNodes' :: Ord a => Gph a -> Set a
startNodes' g
  = Set.fold worker Set.empty topComponents
  where
    (scs,scg) = strongComponentGraph g
    topComponents = (topNodes scg) `Set.union` (scs Set.\\ ent scg)
    worker cmpnt ns = Set.insert (pick_one $ Set.toList cmpnt) ns
    pick_one []    = error "Component should not be empty!!"
    pick_one (x:_) = x      


-- | Works correctly only if all nodes are reachable from top nodes!!
height'' :: Ord a => Gph a -> Int
height'' g = h
 where
  h     = maximum $ (0:) $ Map.elems $ hsMap
  hsMap = heights'' g Set.empty Map.empty (topNodes g)

heights'' :: Ord a => Gph a -> Set a -> Map a Int -> Set a -> Map a Int
heights'' g visited mp tovisit
 = mp'
 where
   mp' = Set.fold worker mp tovisit
   worker n mp = fst $ heights1'' g visited mp n

heights1'' :: Ord a => Gph a -> Set a -> Map a Int -> a -> (Map a Int,Int)
heights1'' g visited mp n
  = maybe (mp'',h) (\h' -> trc h h' (mp,h')) (Map.lookup n mp)
  where 
    trc h h' 
      | h==h' = id
      | otherwise = trace ("Prefer "++(show h')++" over "++(show h))
    mp'' = Map.insert n h mp'
    h = 1 + (maximum $ (0:) hs)
    (mp',hs) = Set.fold worker (mp,[]) kidsNotVisited
    worker k (mp,hs) = (mp',(h:hs)) where (mp',h) = heights1'' g visited' mp k 
    kids = rngWith (Set.singleton n) g
    visited' = Set.insert n visited
    kidsNotVisited = kids Set.\\ visited'

-- | Works for any graph.
height''' :: Ord a => Gph a -> Int
height''' g = h
 where
  h     = maximum $ (0:) $ Set.elems $ hs
  hs    = Set.map (snd . heights1'' g Set.empty Map.empty) (ent g)

-}

-----------------------------------------------------------------------------


