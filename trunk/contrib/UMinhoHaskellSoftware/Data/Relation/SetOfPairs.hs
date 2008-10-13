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
  module Set,
  module Map,
  module Data.SetExtras,
  module Data.Bag
) where

import qualified Data.Set as Set
import Data.Set(Set) -- to have the constructor
import Data.SetExtras
import qualified Data.Map as Map
import Data.Map(Map) -- to have the constructor
import Data.Bag

-----------------------------------------------------------------------------
-- * Representation

-- | Type of relations
type Rel a b = Set (a,b)

-----------------------------------------------------------------------------
-- * Building relations           

-- | Build an empty relation.              
emptyRel        :: Rel a b
emptyRel        = Set.empty

-- | Build a relation from a list of pairs.
mkRel           :: (Ord a, Ord b) => [(a,b)] -> Rel a b
mkRel pairs     = Set.fromList pairs

-- | Build a relation from distributing an element to a set of elements
mkRelNeighbors      :: (Ord a, Ord b) => a -> [b] -> Rel a b
mkRelNeighbors a l  = Set.fromList [ (a, x) | x <- l ]

-- | Build identity relation, which contains an edge from each node to itself.
identityRel      :: Ord a => Set a -> Rel a a
identityRel s    = Set.map (\x -> (x,x)) s

-- | Build total relation, which contains an edge from each node to 
--   each other node and to itself.
totalRel         :: Ord a => Set a -> Rel a a
totalRel s       = Set.fromList [ (x,y) |  x <- l, y <- l ]
                   where
                     l = Set.elems s

-- | Build a chain relation of given number of numerals.
chainRel        :: (Enum n, Num n, Ord n) =>  n -> Rel n n
chainRel n      = Set.fromList (Prelude.map (\i -> (i,i+1)) [1..n])

-- | Build a relation from a predicate
predRel :: Ord a => Set a -> (a -> Bool) -> Rel a a
predRel a p = identityRel $ Set.filter p a 

-- | Build a relation (correflexive) from a set
setRel :: Ord a => Set a -> Rel a a
setRel s = identityRel s

-----------------------------------------------------------------------------
-- * Basic operations

-- | Obtain the domain of a relation
dom             :: (Ord a, Ord b) => Rel a b -> Set a
dom xs          = Set.map fst xs

-- | Obtain the range of a relation
rng             :: (Ord a, Ord b) => Rel a b -> Set b
rng xs          = Set.map snd xs

-- | Obtain the subdomain of a relation given a subrange.
domWith             :: (Ord a, Ord b) => Set b -> Rel a b -> Set a
domWith bs r        = Set.fromList [ a | (a,b) <- Set.elems r , b `Set.member` bs ]

-- | Obtain the subrange of a relation given a subdomain.
rngWith             :: (Ord a, Ord b) => Set a -> Rel a b -> Set b
rngWith as r        = Set.fromList [ b | (a,b) <- Set.elems r , a `Set.member` as ]

-- | Convert relation to a list of pairs.
pairs           :: (Ord a, Ord b) => Rel a b -> [(a,b)]
pairs r         = Set.elems r

-- | Take the inverse of a relation
inv            :: (Ord a, Ord b) => Rel a b -> Rel b a
inv xs         = Set.map (\(x,y) -> (y,x)) xs

-- | Compose two relations
comp            :: (Ord a, Eq b, Ord c) => Rel b c -> Rel a b -> Rel a c
comp yz xy      = Set.fromList 
                    [ (x,z) | (x,y) <- Set.elems xy
                            , (y',z) <- Set.elems yz
                            , y==y'
                    ]

-- | Kernel of a relationship.
ker :: (Ord a, Ord b) => Rel a b -> Rel a a
ker r = inv r `comp` r

-- | Image of a relationaship.
img :: (Ord a, Ord b) => Rel a b -> Rel b b
img r = r `comp` inv r


-- http://www.ics.uci.edu/~eppstein/161/960220.html
    
-- type b :<-: a = Rel a b
-- type a :->: b = Rel a b

-----------------------------------------------------------------------------
-- * Projection

-- | Retrieve a subrelation given predicates on domain and range.
projectWith :: (Ord a, Ord b) 
            => (a -> b -> Bool) 
            -> Rel a b -> Rel a b
projectWith p = Set.filter (uncurry p)

-- | Projection of set through relation
project     :: (Ord a, Ord b) => Set a -> Rel a b -> Rel a b
project s    = Set.filter ( \(x,_) -> Set.member x s ) 

-- | Projection of set backward through relation
projectBackward    :: (Ord a, Ord b) => Set b -> Rel a b -> Rel a b
projectBackward s   = Set.filter ( \(_,y) -> Set.member y s )

-----------------------------------------------------------------------------
