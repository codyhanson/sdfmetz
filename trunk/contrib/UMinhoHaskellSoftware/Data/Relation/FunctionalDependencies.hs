module Data.Relation.FunctionalDependencies where

import Data.Relation.Taxonomy
import Data.Relation.SetOfPairs
import Data.PartialOrder


-- | Type of functional dependencies.
data FunDep a b c d = FunDep { 
  antecedent :: Rel b d, 
  consequent :: Rel a c
 } deriving Show

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


-----------------------------------------------------------------------------


-- | f,g-Projection of relation R

projection :: (Ord a, Ord b, Ord c, Ord d) =>
                   Rel a c-> Rel b d->Rel b a ->Rel d c
projection g f r 
  = (g `comp` r `comp` inv f)

-- | Standard Projection

proj :: (Ord a, Ord b) => Rel a b -> Rel a a -> Rel b b 
proj x = projection x x

