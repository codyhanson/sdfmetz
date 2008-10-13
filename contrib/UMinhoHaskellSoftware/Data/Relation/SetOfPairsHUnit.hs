-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Relation.SetOfPairsHUnit
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Tests for the implementation of relations as sets of pairs.
--
-----------------------------------------------------------------------------

module Data.Relation.SetOfPairsHUnit where

import Data.Set
import Data.Graph.Basics
import HUnit

-----------------------------------------------------------------------------

main
  = runTestTT . TestList $ testList
  
labeledTest l t
  = TestLabel l (TestCase t)

testList
  = [ labeledTest "testEmpty" (
        do let rel = empty :: Set (Int,Int)
           assertEqual "Domain should be empty" empty (dom rel)
           assertEqual "Range should be empty" empty (rng rel)
           assertEqual "Carrier should be empty" empty (ent rel)
      ),
      labeledTest "testSingleton" (
        do let rel = mkRel [(True,False)]
           assertEqual "Domain" (fromList [True]) (dom rel)
           assertEqual "Range" (fromList [False]) (rng rel)
           assertEqual "Carrier" (fromList [True,False]) (ent rel)
      )]

-----------------------------------------------------------------------------

