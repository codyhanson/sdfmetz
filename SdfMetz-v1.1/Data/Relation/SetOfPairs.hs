{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation.SetOfPairs
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- An implementation of relations as sets of pairs.
--
-----------------------------------------------------------------------------

module Data.Relation.SetOfPairs (
  module Data.Relation.SetOfPairs,
  module Data.Set
) where

import Data.Set
import Data.SetExtras
import Data.FiniteMap
import Data.Bag
import Data.List (elemIndex)
import Control.Monad.State

-----------------------------------------------------------------------------

-- * Representation

-- | Type of relations
type Rel a b = Set (a,b)

-- | Type of graphs: relations with domain and range of the same type.
type Gph a = Rel a a       

-- | Type of graphs, with explicit entity set.
type Graph a = (Set a, Gph a)

-----------------------------------------------------------------------------
-- * Building relations           

-- | Build an empty relation.              
emptyRel        :: Rel a b
emptyRel        = emptySet

-- | Build a relation from a list of pairs.
mkRel           :: (Ord a, Ord b) => [(a,b)] -> Rel a b
mkRel pairs     = mkSet pairs

-- | Build a relation from distributing an element to a set of elements
mkRelNeighbors      :: (Ord a, Ord b) => a -> [b] -> Rel a b
mkRelNeighbors a l  = mkSet [ (a, x) | x <- l ]

-- | Build identity relation, which contains an edge from each node to itself.
identityRel      :: Ord a => Set a -> Rel a a
identityRel s    = mapSet (\x -> (x,x)) s

-- | Build total relation, which contains an edge from each node to 
--   each other node and to itself.
totalRel         :: Ord a => Set a -> Rel a a
totalRel s       = mkSet [ (x,y) |  x <- l, y <- l ]
                   where
                     l = setToList s

-- | Build a chain relation of given number of numerals.
chainRel        :: (Enum n, Num n, Ord n) =>  n -> Rel n n
chainRel n      = mkSet (map (\i -> (i,i+1)) [1..n])

-----------------------------------------------------------------------------
-- * Basic operations

-- | Obtain the domain of a relation
dom             :: Ord a => Rel a b -> Set a
dom xs          = mapSet fst xs

-- | Obtain the range of a relation
rng             :: Ord b => Rel a b -> Set b
rng xs          = mapSet snd xs

-- | Obtain all entities in a relation (union of domain and range)
ent             :: Ord a => Rel a a -> Set a
ent xs          = dom xs `union` rng xs

-- | Convert relation to a list of pairs.
pairs           :: (Ord a, Ord b) => Rel a b -> [(a,b)]
pairs r         = setToList r

-- | Take the inverse of a relation
inv            :: (Ord a, Ord b) => Rel a b -> Rel b a
inv xs         = mapSet (\(x,y) -> (y,x)) xs

-- | Compose two relations
comp            :: (Ord a, Eq b, Ord c) => Rel b c -> Rel a b -> Rel a c
comp yz xy      = mkSet 
                    [ (x,z) | (x,y) <- setToList xy
                            , (y',z) <- setToList yz
                            , y==y'
                    ]

-----------------------------------------------------------------------------
-- * Closures

-- | Compute the reflective closure
reflClose       :: Ord a => Rel a a -> Rel a a
reflClose xs    = xs `union` mapSet (\x -> (x,x)) (ent xs)

-- | Compute the symmetric closure
symmClose       :: Ord a => Rel a a -> Rel a a
symmClose xs    = xs `union` mapSet (\(x,y) -> (y,x)) xs
            
-- | Compute the transitive closure (naive, slow)
transClose'       :: Ord a => Rel a a -> Rel a a
transClose' xs    = if xxs == xs then xs else (transClose' xxs)
                   where 
                     xxs = union xs (comp xs xs)

-- | Compute the transitive closure (faster)
transClose       :: Ord a => Rel a a -> Rel a a
transClose xs
  = mkSet $ concatMap tcFrom [ x | (x,_) <- setToList xs ]
    where 
      tcFrom x = [ (x,y) | y <- setToList (reachableFrom x) ]
      reachableFrom x
        = rng (slice (unitSet x) xs)

-- | Compute the reflexive, transitive closure
reflTransClose    :: Ord a => Rel a a -> Rel a a
reflTransClose xs = reflClose (transClose xs)

-- | Compute the reflexive, transitive, symmetric closure, i.e. the
--   induced equivalence relation.
equiv      :: Ord b => Rel b b -> Rel b b
equiv r    = reflTransClose (r `union` (inv r))

-----------------------------------------------------------------------------
-- * Reductions

-- | Reflexive reduction, i.e remove self-edges.
reflReduc :: Ord a => Gph a -> Gph a
reflReduc g = mkRel $ filter (\(x,y) -> x /= y) $  setToList g

-----------------------------------------------------------------------------
-- * Basic graph queries

-- | Find top nodes of a graph. Those nodes that have outgoing but no
--   incoming edges. 
topNodes :: Ord a => Gph a -> Set a
topNodes g = dom g `minusSet` rng g

-- | Find bottom nodes of a graph. Those nodes that have incoming but no
--   outgoing edges. 
bottomNodes :: Ord a => Gph a -> Set a
bottomNodes g = rng g `minusSet` dom g

-- | Find internal nodes of a graph. Those nodes that have both 
--   incoming and outgoing edges. 
internalNodes :: Ord a => Gph a -> Set a
internalNodes g = rng g `intersect` dom g

-----------------------------------------------------------------------------
-- * Selections (project, slice, chop)

-- | Obtain the subdomain of a relation given a subrange.
domWith             :: (Ord a, Ord b) => Set b -> Rel a b -> Set a
domWith bs r        = mkSet [ a | (a,b) <- setToList r , b `elementOf` bs ]

-- | Obtain the subrange of a relation given a subdomain.
rngWith             :: (Ord a, Ord b) => Set a -> Rel a b -> Set b
rngWith as r        = mkSet [ b | (a,b) <- setToList r , a `elementOf` as ]

-- | Obtain the subrange and subdomain of a relation given a subdomain.
entWith             :: Ord a => Set a -> Rel a a -> Set a
entWith as r        = domWith as r `union` rngWith as r

-- | Remove all edges of the form (x,x).
removeSelfEdges :: Ord a => Gph a -> Gph a
removeSelfEdges r
  = mkRel [ (x,y) | (x,y) <- setToList r, x /= y ]
  
-- | Retrieve a subrelation given predicates on domain and range.
projectWith :: (Ord a, Ord b) 
            => (a -> b -> Bool) 
            -> Rel a b -> Rel a b
projectWith p r
            = mkSet [ (x,y) | (x,y) <- setToList r, p x y]

-- | Projection of set through relation
project     :: (Ord a, Ord b) => Set a -> Rel a b -> Rel a b
project    s r     = mkSet [ (x,y) | (x,y) <- setToList r, x `elementOf` s ]

-- | Projection of set backward through relation
projectBackward    :: (Ord a, Ord b) => Set b -> Rel a b -> Rel a b
projectBackward    s r
             = mkSet [ (x,y) | (x,y) <- setToList r, y `elementOf` s ]

-- | Compute forward slice starting from seeds.
slice            :: Ord a => Set a -> Rel a a -> Rel a a
slice seed r        = sliceUntil seed emptySet r

-- | Compute forward slice starting from seeds, stopping at stoppers.
sliceUntil        :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
sliceUntil seed stop r    
                = if (isEmptySet s) then emptyRel 
                                    else r' `union`
                                         (sliceUntil seed' stop' r)
              where
                s = seed `minusSet` stop    -- live seeds
                r' = project s r            -- step
                seed' = rng r'              -- new seeds
                stop' = stop `union` s      -- extend stoppers

-- | Compute backward slice starting from seeds.
sliceBackward        :: Ord a => Set a -> Rel a a -> Rel a a
sliceBackward seed r    = inv (slice seed (inv r))

-- | Compute chop between seeds and sinks.
chop                    :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
chop seeds sinks r  = slicesWith intersect seeds sinks r
                      
-- | Compute union of backward and forward slice.
slicesUnion :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
slicesUnion seeds sinks r
  = (slice seeds r) `union` (sliceBackward sinks r)

-- | Compute intersection of backward and forward slice.
--   This is the same as the computing the chop between seeds and sinks.
slicesIntersect :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
slicesIntersect seeds sinks r  
  = (slice seeds r) `intersect` (sliceBackward sinks r)
  
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
sliceOrChopWith binop [] sinks rel      = sliceBackward (mkSet sinks) rel
sliceOrChopWith binop sources [] rel    = slice (mkSet sources) rel
sliceOrChopWith binop sources sinks rel 
  = slicesWith binop (mkSet sources) (mkSet sinks) rel
      
-----------------------------------------------------------------------------
-- * Partitions

-- | Compute subrelation connected to seeds, stopping at stoppers.
gobbleUntil        :: Ord a => Set a -> Set a -> Rel a a -> Rel a a
gobbleUntil seed stop r    
                = if (isEmptySet s) then emptyRel 
                                    else r' `union`
                                         (gobbleUntil seed' stop' r)
              where
                s = seed `minusSet` stop    -- live seeds
                r' = (project s r) `union` (projectBackward s r)   -- step
                seed' = ent r'              -- new seeds
                stop' = stop `union` s      -- extend stoppers

-- | Compute weakly connected components.
weakComponents :: Ord a => Gph a -> [Gph a]
weakComponents r
  = weakComponentsUntil emptySet r
    where
      weakComponentsUntil done r
        = case setToList ((ent r) `minusSet` done) of
           []    -> []
           (a:_) -> (r':s)
                    where
                      r' = gobbleUntil (unitSet a) emptySet r
                      s  = weakComponentsUntil (done `union` (ent r')) r   

-- | Compute weakly connected components (sets of nodes)
weakComponentSet r
 = mkSet (map ent $ weakComponents r)

-- | Compute a single strong component (set of nodes).
strongComponent :: (Ord a) => a -> Gph a -> Set a
strongComponent x r
  = addToSet reach x
    where
      reach = (reachableFrom x r) `intersect` (reachableFrom x (inv r))
      reachableFrom x r
        = ent (slice (unitSet x) r)
        
-- | Compute the set of strong components (sets of nodes).
strongComponentSet :: (Ord a, Ord (Set a)) => Gph a -> Set (Set a)
strongComponentSet r
  = mapSet (\x -> strongComponent x r) (ent r)

-- | Type of components: node set tupled with subgraph.
type Component a = Graph a

-- | Compute a single strong component (node set tupled with subrelation).
strongComponent' :: (Ord a) => a -> Rel a a -> Component a
strongComponent' x r
  = (scc,subrel)
    where
      scc = strongComponent x r
      subrel = mkRel [ (x,y) | (x,y) <- setToList r, 
                               x `elementOf` scc, y `elementOf` scc ]

-- | Compute the graph of strong components (node sets).
--   Note: strong components that have no relations to other components
--   will not be present in the graph!
strongComponentRel :: (Ord a, Ord (Set a)) => Gph a -> Gph (Set a) 
strongComponentRel r
  = removeSelfEdges $ mapSet edgeT r
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = (strongComponent x r)

-- | Compute the graph of strong components (node sets).
strongComponentGraph :: (Ord a, Ord (Set a)) => Gph a -> Graph (Set a) 
strongComponentGraph r
  = (st, removeSelfEdges gph)
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = lookupWithDefaultFM mp (error "SCC map misses entry") x
      gph = mapSet edgeT r
      st = mkSet $ eltsFM mp
      -- Using finite map from nodes to SSCs for performance reasons:
      mp = listToFM $ map (\x -> (x, strongComponent x r))  $ setToList $ ent r
      
-- | Compute the graph of stong components (node sets tupled with subrelations).
strongComponentRel' :: (Ord a, Ord (Set a)) 
                    => Gph a -> Gph (Component a)
strongComponentRel' r
  = removeSelfEdges $ mapSet edgeT r
    where
      edgeT (x,y) = (nodeT x, nodeT y)
      nodeT x = strongComponent' x r

-- | Compute the set of strong components that have more than a single node
strongNonSingletonComponentSet :: (Ord a, Ord (Set a)) 
                               => Gph a -> Set (Set a)
strongNonSingletonComponentSet
  = filterSet (\c -> cardinality c > 1) . strongComponentSet
    
-----------------------------------------------------------------------------
-- * Integration

-- | Integrate two graphs with respect to a base graph into a
--   new graph that contains the differences of each input graph with
--   respect to the base graph. The boolean value indicates whether the
--   graphs are interference-free, i.e. whether the integration is valid. 
integrate :: Ord a => Rel a a -> Rel a a -> Rel a a -> (Rel a a,Bool)
integrate a b base
  = (m,ok)
    where
      apa = affectedPoints a base
      apb = affectedPoints b base
      aapa = a // apa
      bapb = b // apb
      basepp  = base // (preservedPoints a b base)
      m = aapa `union` bapb `union` basepp
      ok = m//apa == aapa && m//apb == bapb
      g / v = sliceBackward (unitSet v) g
      g // s = sliceBackward s g

-- | Points in the first graph that are affected by changes with respect
--   to the base graph.
affectedPoints :: Ord a => Rel a a -> Rel a a -> Set a
affectedPoints x base
        = mkSet [ v | v <- setToList (ent x), (base/v) /= (x/v) ]
          where
            g / v = sliceBackward (unitSet v) g

-- | Points in the base graph that are not affected by changes in either
--   input graph.
preservedPoints ::  Ord a => Rel a a -> Rel a a -> Rel a a -> Set a
preservedPoints x y base
        = mkSet [ v | v <- setToList (ent base), 
                      let  base' = base/v in base' == (x/v) && base' == (y/v)
                ]
          where
            g / v = sliceBackward (unitSet v) g
            
-----------------------------------------------------------------------------
-- * Structure metrics

-- | Tree impurity (TIMP): 
--   how much does the graph resemble a tree, expressed as
--   a percentage. A tree has tree impurity of 0 percent. A fully connected
--   graph has tree impurity of 100 percent. Fenton and Pfleeger only
--   defined tree impurity for non-directed graphs without self-edges. This
--   implementation therefore does not count self-edges, and counts 2 directed
--   edges between the same nodes only once.
treeImpurity :: Ord a => Gph a -> Float
treeImpurity g 
  | n==2 || n==1
  -- In these cases, the max number of edges more than the spanning tree
  -- is 0, so the tree impurity should also be 0.
  = 0
  | otherwise
  = (2 * (e - n + 1)) / ((n - 1) * (n - 2)) * 100
  where e = (realToFrac $ cardinality $ reflReduc $ symmClose g) / 2
        n = realToFrac $ cardinality $ ent g


-- | Tree impurity after transitive closure:
-- This metric differs computes tree impurity not on the graph
--   itself, but on its transitive closure.
treeImpurityTC :: Ord a => Gph a -> Float
treeImpurityTC g = treeImpurity $ transClose g

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
calculateStrongComponentMetrics g = StrongComponentMetrics {
   componentCount = l,
   componentCountNormalized = l `asPercentageOf` n,
   nonSingletonComponentCount = cardinality sNSCS,
   componentSizeMax = maximum $ (0:) $ map cardinality $ setToList sCS,
   heightOfComponentGraph = heightDag sCR
 } where
    sCG = strongComponentGraph g
    sCS = fst sCG
    sCR = snd sCG
    sNSCS = filterSet (\c -> cardinality c > 1) sCS
    l = cardinality sCS
    n = cardinality $ ent g

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
  
-- | Height (works also in the presence of cycles)
height :: Ord a => Gph a -> Int
height g 
  = (heightFrom newG emptySet (unitSet Nothing)) - 1
  where
    justG = mapSet (\(x,y)->(Just x, Just y)) g
    newEdges = mapSet (\x -> (Nothing, x)) (dom justG)
    newG = newEdges `union` justG 

-- | Height (does not work in the presence of cycles)
heightDag :: Ord a => Gph a -> Int
heightDag g = heightFrom g emptySet (topNodes g)

-- | Compute graph height starting from a given set of nodes.
heightFrom :: Ord a 
           => Gph a        -- ^ Graph
           -> Set a        -- ^ Already visited nodes
           -> Set a        -- ^ Nodes still to visit
           -> Int          -- ^ Result
heightFrom g visited tovisit 
  = maximum $ (0:) $ map (+1) $ setToList heights
  where
    currentNodes = tovisit `minusSet` visited
    heights = mapSet heightFrom1 currentNodes
    heightFrom1 n = heightFrom g (addToSet visited n) (rngWith (unitSet n) g)

-- | Compute fan-in and fan-out of a given graph. Both are represented
--   with a bags of nodes. The arity of each node in the bag is its
--   fanout or fanin, repectively.
fanInOut :: Ord a => Gph a -> (Bag a, Bag a)
fanInOut g = foldr fan1 (emptyBag,emptyBag) $ setToList g
  where
    fan1 (a,b) (fanIn,fanOut)
      = (addToBag fanIn b, addToBag fanOut a)

-- | Helper for math.
asPercentageOf :: Int -> Int -> Float
asPercentageOf _ 0 = 0
asPercentageOf l n = (realToFrac l / realToFrac n) * 100

-----------------------------------------------------------------------------
-- | Traversal (under construction)

dfs r
  = dfsFrom r [] (setToList $ ent r)
    

dfsFrom r stop []
  = []
dfsFrom r stop (n:ns)
  | n `elem` stop
  = dfsFrom r stop ns
  | otherwise
  = fromKids++(n:(dfsFrom r stop'' ns))
    where
      kids = [ y | (x,y) <- setToList r, x==n ]
      stop' = n:stop
      fromKids = dfsFrom r stop' kids
      stop'' = stop'++fromKids
      
dfsFrom' r (stop,(n:ns))
  | n `elem` stop
  = dfsFrom' r (stop,ns)
  | otherwise
  = (stop''',fromKids++(n:fromRest))
    where
      kids = [ y | (x,y) <- setToList r, x==n ]
      stop' = n:stop
      (stop'',fromKids)  = dfsFrom' r (stop',kids)
      (stop''',fromRest) = dfsFrom' r (stop'',ns)
dfsFrom' r (stop,[])
  = (stop,[])
            
dfs' r
  = snd $ dfsFrom' r ([],(setToList $ ent r))
   
dfsS r
  = evalState (dfsState r) ([],setToList $ ent r)
dfsState r  = do
  (stop,ns) <- get
  case ns of
    (n:ns)
      -> if (n `elem` stop)
          then do
            put (stop,ns)
            dfsState r
          else do
            let kids = [ y | (x,y) <- setToList r, x==n ]
            put (n:stop,kids)
            fromKids <- dfsState r
            (stop,_) <- get
            put (stop,ns)
            fromRest <- dfsState r
            return (fromKids++(n:fromRest))
    []
      -> return ns

dfsT r
  = evalState (dfsStateT r) ([],setToList $ ent r,emptyRel)
dfsStateT r  = do
  (stop,ns,t) <- get
  case ns of
    (n:ns)
      -> if (n `elem` stop)
          then do
            put (stop,ns,t)
            dfsStateT r
          else do
            let entT = ent t
            let edges = [ (x,y) | (x,y) <- setToList r, 
                                  x==n, not (y `elementOf` entT) ]
            let kids = map snd edges
            put (n:stop,kids,t `union` (mkRel edges))
            dfsStateT r
            (stop,_,t) <- get
            put (stop,ns,t)
            dfsStateT r
    []
      -> return t


-- http://www.ics.uci.edu/~eppstein/161/960220.html

-----------------------------------------------------------------------------
-- * Point free relations

-- See 
--   Functional dependency theory made ÕsimplerÕ--   J.N. Oliveira
--   Techn. Report DI-PURe-05.01.01--   2005, January

-- | Kernel of a relationship.
ker :: (Ord a, Ord b) => Rel a b -> Rel a a
ker r = inv r `comp` r

-- | Image of a relationaship.
img :: (Ord a, Ord b) => Rel a b -> Rel b b
img r = r `comp` inv r

-- | Test whether relation is reflexive.
isReflexive :: Ord a => Rel a a -> Bool
isReflexive r = id .<=. r
  where id = identityRel (ent r)

-- | Test whether relation is coreflexive.
isCoreflexive :: Ord a => Rel a a -> Bool
isCoreflexive r = r .<=. id
  where id = identityRel (ent r)

-- | Test whether relation is simple.
isSimple :: (Ord a, Ord b) => Rel a b -> Bool
isSimple r = isCoreflexive (img r)

-- | Test whether relation is entire.
isEntire :: (Ord a, Ord b) => Rel a b -> Bool
isEntire r = isReflexive (ker r)

-- | Test whether relation is injective.
isInjective :: (Ord a, Ord b) => Rel a b -> Bool
isInjective r = isCoreflexive (ker r)

-- | Test whether relation is surjective.
isSurjective :: (Ord a, Ord b) => Rel a b -> Bool
isSurjective r = isReflexive (img r)

-- | Test whether relation is functional.
isRepresentation :: (Ord a, Ord b) => Rel a b -> Bool
isRepresentation r = isInjective r && isEntire r 

-- | Test whether relation is functional.
isFunction :: (Ord a, Ord b) => Rel a b -> Bool
isFunction r = isEntire r && isSimple r 

-- | Test whether relation is functional.
isAbstraction :: (Ord a, Ord b) => Rel a b -> Bool
isAbstraction r = isSimple r && isSurjective r 

-- | Test whether relation is injective.
isInjection :: (Ord a, Ord b) => Rel a b -> Bool
isInjection r = isRepresentation r && isFunction r

-- | Test whether relation is surjective.
isSurjection :: (Ord a, Ord b) => Rel a b -> Bool
isSurjection r = isFunction r && isAbstraction r

-- | Test whether relation is surjective.
isBijection :: (Ord a, Ord b) => Rel a b -> Bool
isBijection r = isInjection r && isSurjection r

predRel :: Ord a => Set a -> (a -> Bool) -> Rel a a
predRel a p = identityRel $ mkSet $ filter p $ setToList a 

setRel :: Ord a => Set a -> Rel a a
setRel s = identityRel s

-- | Type of functional dependencies.
data FunDep a b c d = FunDep { 
  antecedent :: Rel b d, 
  consequent :: Rel a c
 }

-- | Create a functional dependency from two functions
--   and a relation.
mkFunDep :: (Ord a, Ord b, Ord c, Ord d) 
         => (b -> d) -> (a -> c) -> Rel b a 
         -> FunDep a b c d 
mkFunDep g f rel = FunDep gRel fRel
  where fRel = mkRel [ (b,f b) | (_,b) <- pairs rel ]
        gRel = mkRel [ (a,g a) | (a,_) <- pairs rel ]

-- | Test satisfaction of a functional dependency by
--   a relation.
satisfiesFunDep :: (Ord a, Ord b, Ord c, Ord d) =>
                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep fd r 
  = isSimple (g `comp` r `comp` inv f)
    where f = antecedent fd
          g = consequent fd

-- | Test satisfaction of a functional dependency by
--   a relation (alternative formulation).
satisfiesFunDep' :: (Ord a, Ord b, Ord c, Ord d) =>
                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep' fd r 
  = r `comp` ker f `comp` inv r .<=. ker g
    where f = antecedent fd
          g = consequent fd

-- | Test satisfaction of a functional dependency by
--   a relation (another alternative formulation).
--satisfiesFunDep'' :: (Ord a, Ord b, Ord c, Ord d) =>
--                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep'' fd r 
  = ker (f `comp` inv r) .<=. ker g
    where f = antecedent fd
          g = consequent fd

-- | Test satisfaction of a functional dependency by
--   a relation (yet another alternative formulation).
satisfiesFunDep''' :: (Ord a, Ord b, Ord c, Ord d) =>
                   FunDep a b c d -> Rel b a -> Bool
satisfiesFunDep''' fd r 
  = isSimple $ projection g f r
    where f = antecedent fd
          g = consequent fd


-- | Test whether a given projection is a super key for
--   a given relation.
isSuperKey :: (Ord a, Ord b, Ord d) => Rel b d -> Rel b a -> Bool
isSuperKey x r
  = satisfiesFunDep (FunDep x id) r
    where id = identityRel (rng r)
    
type b :<-: a = Rel a b
type a :->: b = Rel a b

class PartialOrd a where
  maybeCompare :: a -> a -> Maybe Ordering
  (.<=.) :: a -> a -> Bool
  x .<=. y = maybe False (/=GT) $ maybeCompare x y
  (.>.)  :: a -> a -> Bool
  x .>. y = maybe False (==GT) $ maybeCompare x y
  (.>=.) :: a -> a -> Bool
  x .>=. y = maybe False (/=LT) $ maybeCompare x y
  (.<.)  :: a -> a -> Bool
  x .<. y = maybe False (==LT) $ maybeCompare x y
  (.==.)  :: a -> a -> Bool
  x .==. y = maybe False (==EQ) $ maybeCompare x y
  (./=.)  :: a -> a -> Bool
  x ./=. y = maybe False (/=EQ) $ maybeCompare x y
  (.||.)  :: a -> a -> Bool
  x .||. y = maybe True (const False) $ maybeCompare x y

instance Ord a => PartialOrd (Set a) where
  maybeCompare x y = case (isEmptySet xMy, isEmptySet yMx) of
      (False,False) -> Nothing
      (True,False)  -> Just LT
      (False,True)  -> Just GT
      (True,True)   -> Just EQ
    where xMy = x `minusSet` y
          yMx = y `minusSet` x
    
-----------------------------------------------------------------------------

-- | f,g-Projection of relation R

projection :: (Ord a, Ord b, Ord c, Ord d) =>
                   Rel a c-> Rel b d->Rel b a ->Rel d c
projection g f r 
  = (g `comp` r `comp` inv f)

-- | Standard Projection

proj :: (Ord a, Ord b) => Rel a b -> Rel a a -> Rel b b 
proj x = projection x x

-- | Type of a relation's scheme

type Scheme = [String]

-- | Standard projection given the attributte name and the relation scheme
    --TO DO: We should check if the schema agrees with the given Relation
    --       : It is only working for 4 attribbutes scheme
{-
projAtributte :: (Ord a, Ord b) => String -> Scheme -> Rel a a -> Rel b b 
projAtributte s schema r = let pos = elemIndex s schema
                         in case pos of
                               Nothing -> error "That attributte is not in the schema of the given table"
                               Just x -> proj  (mkRel [(a, select x a)  | (a,_) <- pairs r]) r


select 1 (x,_,_,_) = x
select 2 (_,x,_,_) = x
select 3 (_,_,x,_) = x
select 4 (_,_,_,x) = x
select _ _ = error "component out of range"
-}
