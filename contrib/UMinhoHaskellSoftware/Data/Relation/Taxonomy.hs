
module Data.Relation.Taxonomy where

import Data.PartialOrder
import Data.Graph.Basics
import Data.Relation.SetOfPairs
-----------------------------------------------------------------------------
-- * Point free relations

-- See 
--   Functional dependency theory made ÕsimplerÕ--   J.N. Oliveira
--   Techn. Report DI-PURe-05.01.01--   2005, January

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

-- | Test whether relation is a representation.
isRepresentation :: (Ord a, Ord b) => Rel a b -> Bool
isRepresentation r = isInjective r && isEntire r 

-- | Test whether relation is functional.
isFunction :: (Ord a, Ord b) => Rel a b -> Bool
isFunction r = isEntire r && isSimple r 

-- | Test whether relation is an abstraction.
isAbstraction :: (Ord a, Ord b) => Rel a b -> Bool
isAbstraction r = isSimple r && isSurjective r 

-- | Test whether relation is an injective function.
isInjection :: (Ord a, Ord b) => Rel a b -> Bool
isInjection r = isRepresentation r && isFunction r

-- | Test whether relation is a surjective function.
isSurjection :: (Ord a, Ord b) => Rel a b -> Bool
isSurjection r = isFunction r && isAbstraction r

-- | Test whether relation is an isomorphism.
isBijection :: (Ord a, Ord b) => Rel a b -> Bool
isBijection r = isInjection r && isSurjection r


