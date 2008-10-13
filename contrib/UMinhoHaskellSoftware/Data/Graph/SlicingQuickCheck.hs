module Data.Graph.SlicingQuickCheck where

import Data.Graph.Slicing
import Data.Set
import Data.Relation
import Test.QuickCheck
import Data.Relation.SetOfPairsQuickCheck ()



prop_ProjectInv s r
  = classify (size s == 0 && size r == 0) "both empty" $
    classify (size s /= 0 && size r /= 0) "none empty" $
    project s r == inv (projectBackward s (inv r))
    where
      types = (s::Set Int, r::Rel Int Integer)

prop_ChopInv seeds sinks r
  = classify (size seeds /= 0 && size sinks /= 0 && size r /= 0)
        "none empty" $
    chop seeds sinks r == inv (chop sinks seeds (inv r))
    where
      types = (r::Rel Int Int)

prop_SliceIdentity s
  = classify (size s /= 0) "non-empty" $
    slice s (identityRel s) == (identityRel s)
    where
      types = (s::Set Int)

prop_SliceTotal s
  = classify (size s /= 0) "non-empty" $
    slice s (totalRel s) == (totalRel s)
    where
      types = (s::Set Int)

prop_SliceIntersect seeds sinks r
  = classify noneEmpty "none empty" $
    classify resultNonEmpty "result non-empty" $
    slicetwice == intersectslices
    where
      slicetwice = sliceBackward sinks (slice seeds r)
      intersectslices = (slice seeds r) `intersection` (sliceBackward sinks r)
      noneEmpty = size seeds /= 0 && size sinks /= 0 && size r /= 0
      resultNonEmpty = size slicetwice /= 0
      types = (r::Rel Int Int)
  

