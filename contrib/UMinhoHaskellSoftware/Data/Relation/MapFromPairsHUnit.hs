-----------------------------------------------------------------------------
-- |
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

module Data.Relation.MapFromPairsHUnit where

import qualified Data.Map as Map
import Data.Relation.MapFromPairs
import HUnit

-----------------------------------------------------------------------------

main
  = runTestTT . TestList $ testList
  
labeledTest l t
  = TestLabel l (TestCase t)

testList
  = [ labeledTest "testLeastCost" (
        do let rel = Map.fromList [(("a","b1"),1::Integer),
                               (("a","b2"),3),
                               (("b1","c"),4),
                               (("b2","c"),3)]
           assertEqual "least cost" 
             (Map.fromList [(("a","c"),5)])
             ( (Map.filter (\e -> e < 100) (leastcost 100 rel)) 
               `Map.difference` rel)
           assertEqual "bottle" 
             (Map.fromList [(("a","c"),3)])
             ( (Map.filter (\e -> e < 100 && e > 0) (bottleneck rel)) 
               `Map.difference` rel)
      )
    ]


-----------------------------------------------------------------------------
