module Data.Relation.ClosuresQuickCheck where

import Data.Relation.Closures
import Data.Graph.Basics
import Data.Relation.SetOfPairs
import Data.Relation.SetOfPairsQuickCheck () -- just instances
import Data.Set
import Test.QuickCheck

-----------------------------------------------------------------------------


-- | 
close_prop close r 
  = classify (size r == 0) "empty" $
    close r == close (close r)
    where
      types = r:: Rel Int Int
      
prop_ReflClose r
  = close_prop reflClose r
  
prop_SymmClose r
  = close_prop symmClose r
  
prop_TransClose r
  = close_prop transClose r

prop_ReflTransClose r
  = close_prop reflTransClose r

close_prop_Ent close r 
  = classify (size r == 0) "empty" $
    ent r == ent (close r)
    where
      types = r:: Rel Int Int

prop_RelfCloseEnt r
  = close_prop_Ent reflClose r  
      
prop_SymmCloseEnt r
  = close_prop_Ent symmClose r      
  
prop_TransCloseEnt r
  = close_prop_Ent transClose r    
    
prop_ReflTransCloseEnt r
  = close_prop_Ent reflTransClose r      

prop_ReflCloseDomRng r
  = classify (size r == 0) "empty" $
    dom (reflClose r) == rng (reflClose r)
    where
      types = r:: Rel Int Int  
      
prop_ReflCloseDomEnt r
  = classify (size r == 0) "empty" $
    dom (reflClose r) == ent r
    where
      types = r:: Rel Int Int     



