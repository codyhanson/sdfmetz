
module Data.Relation.ComponentsHUnit where

import Data.Graph.Components
import Data.Relation.SetOfPairs
import Data.Set
import HUnit

-----------------------------------------------------------------------------

main
  = runTestTT . TestList $ testList
  
labeledTest l t
  = TestLabel l (TestCase t)

testList
  = [ 
       labeledTest "test strong components" $ do
         let g = mkRel [("a","a")]
         assertEqual "Single singleton component" 
           (singleton $ singleton "a") (strongComponentSet g)
         assertEqual "Single singleton component" 
           emptyRel (snd $ strongComponentGraph g),
           
       labeledTest "test single strong component" $ do
         let g = mkRel [("a","a"),("a","b"),("b","a")]
             expectedSet = singleton $ fromList ["a","b"]
             expectedRel = emptyRel
         assertEqual "strong component set"
           expectedSet (strongComponentSet g)
         assertEqual "strong component rel"
           expectedRel (snd $ strongComponentGraph g) 
         assertEqual "strong component graph"
           (expectedSet,expectedRel) (strongComponentGraph g)
           
     ]
