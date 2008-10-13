module Data.Relation.SlicingHUnit where

import Data.GraphR
import Data.Relation
import Data.Set
import HUnit

-----------------------------------------------------------------------------

main
  = runTestTT . TestList $ testList
  
labeledTest l t
  = TestLabel l (TestCase t)

testList
  = [    labeledTest "testSliceDiamond" (
        do let diamondRel = mkRel [("a","b1"),("a","b2"),("b1","c"),("b2","c")]
           assertEqual "slice from top" 
             diamondRel 
             (slice (fromList ["a"]) diamondRel) 
           assertEqual "slice from corner" 
             (mkRel [("b1","c")])
             (slice (fromList ["b1"]) diamondRel) 
      ),
      labeledTest "testSliceDiamondCross" (
        do let rel = mkRel [("a","b1"),("a","b2"),("b1","c"),("b2","c"),
                            ("c","d1"),("c","d2")]
           assertEqual "slice from top" 
             rel 
             (slice (fromList ["a"]) rel) 
           assertEqual "slice from corner" 
             (mkRel [("b1","c"),("c","d1"),("c","d2")])
             (slice (fromList ["b1"]) rel) 
      ),
      labeledTest "testChopDiamondCross" (
        do let rel = mkRel [("a","b1"),("a","b2"),("b1","c"),("b2","c"),
                            ("c","d1"),("c","d2")]
           assertEqual "chop top and sinks" 
             rel 
             (chop (fromList ["a"]) (fromList ["d1","d2"]) rel) 
           assertEqual "chop between corner and sink" 
             (mkRel [("b1","c"),("c","d1")])
             (chop (fromList ["b1"]) (fromList ["d1"]) rel) 
      )] 
