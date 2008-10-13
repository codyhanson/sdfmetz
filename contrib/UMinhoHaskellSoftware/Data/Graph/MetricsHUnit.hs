module Data.Relation.MetricsHUnit where

import Data.GraphR
import Data.Relation.SetOfPairs
import Data.Set as Set
import Data.Bag
import Data.Map as Map
import HUnit

-----------------------------------------------------------------------------

main
  = runTestTT . TestList $ testList
  
labeledTest l t
  = TestLabel l (TestCase t)

testList
  = [ labeledTest "testTreeImpurity" $ do
        assertEqual "empty" 0 $ treeImpurity
          (emptyRel :: Gph String)
        assertEqual "1 edge" 0.0 $ treeImpurity $ 
          mkRel [("a","b")]
        assertEqual "tree" 0.0 $ treeImpurity $ 
          mkRel [("a","b"),("a","c")]
        assertEqual "fully connected" 100.0 $ treeImpurity $ 
          totalRel $ Set.fromList ['a'..'f'] 
        assertEqual "self edge" 0 $ treeImpurity $ 
          mkRel [("a","a")] 
        assertEqual "diamond" (2/6*100) $ treeImpurity $ 
          mkRel [("a","b"),("a","c"),("b","d"),("c","d")] 
      ,
      labeledTest "test CountOfLevels" $ do
        assertEqual "empty" 0 $ countOfLevels $
          (emptyRel :: Gph String)
        assertEqual "fully connected" 1 $ countOfLevels $
          totalRel $ Set.fromList ['1'..'6']
        assertEqual "turnstile" 3 $ countOfLevels $
          mkRel [("a","b"),("b","c"),("b","d"),("d","b")]
      ,
      labeledTest "test NormalizedCountOfLevels" $ do
        assertEqual "empty" 0 $ normalizedCountOfLevels $
          (emptyRel :: Gph String)
        assertEqual "fully connected" ((1/6) * 100) $ normalizedCountOfLevels $
          totalRel $ Set.fromList ['1'..'6'] 
        assertEqual "turnstile" ((3/4) * 100) $ normalizedCountOfLevels $
          mkRel [("a","b"),("b","c"),("b","d"),("d","b")]
       , 
       labeledTest "test Number Of Non-singleton Levels" $ do
         assertEqual "empty" 0 $ numberOfNonSingletonLevels $
           (emptyRel :: Gph String)
         assertEqual "chain" 0 $ numberOfNonSingletonLevels $
           (chainRel 6 :: Gph Int)
         assertEqual "turnstile" 1 $ numberOfNonSingletonLevels $
          mkRel [("a","b"),("b","c"),("b","d"),("d","b")]
       , 
       labeledTest "test Size of Largest Level" $ do
         assertEqual "empty" 0 $ sizeOfLargestLevel $
           (emptyRel :: Gph String)
         assertEqual "chain" 1 $ sizeOfLargestLevel $
           (chainRel 6 :: Gph Int)
         assertEqual "turnstile" 2 $ sizeOfLargestLevel $
          mkRel [("a","b"),("b","c"),("b","d"),("d","b")]
       ,
       labeledTest "test graph height" $ do
         assertEqual "empty" 0 $ heightSlow $
           (emptyRel :: Gph String)
         assertEqual "chain" (6+1) $ heightSlow $
           (chainRel 6 :: Gph Int)
         assertEqual "diamond" 3 $ heightSlow $ 
           mkRel [("a","b"),("a","c"),("b","d"),("c","d")]
         assertEqual "Spoon" 3 $ height $
           mkRel [("a","b"),("b","c"),("d","c")]
         assertEqual "cycle of 2" 2 $ heightSlow $
           mkRel [("a","b"),("b","a")]
       ,
       labeledTest "test longestPath (same as height)" $ do
         assertEqual "empty" 0 $ height $
           (emptyRel :: Gph String)
         assertEqual "chain" (6+1) $ height $
           (chainRel 6 :: Gph Int)
         assertEqual "diamond" 3 $ height $ 
           mkRel [("a","b"),("a","c"),("b","d"),("c","d")]
         assertEqual "Spoon" 3 $ height $
           mkRel [("a","b"),("b","c"),("d","c")]
         assertEqual "cycle of 2" 2 $ height $
           mkRel [("a","b"),("b","a")]
       ,
       labeledTest "test fan in and out" $ do
         let g = mkRel [("a","a"),("a","b"),("b","c")]
         let (fanIn,fanOut) = fanInOut g
         assertEqual ".." 0 (lookupBag fanIn "x")
         assertEqual ".." 0 (lookupBag fanOut "x")
         assertEqual ".." 1 (lookupBag fanIn "a")
         assertEqual ".." 2 (lookupBag fanOut "a")
         assertEqual ".." 1 (lookupBag fanIn "b")
         assertEqual ".." 1 (lookupBag fanOut "b")
         assertEqual ".." 1 (lookupBag fanIn "c")
         assertEqual ".." 0 (lookupBag fanOut "c")
       ,
       labeledTest "test coupling" $ do
         let g = mkRel [("a","a"),("a","b"),("b","c")]
         assertEqual ".." (0,0,0,0,0) (coupling Set.empty g)
         assertEqual ".." (1,0,1,100,100) (coupling (Set.singleton "a") g)
         assertEqual ".." (1,1,0,50,100) (coupling (Set.singleton "b") g)
         assertEqual ".." (0,1,0,0,100) (coupling (Set.singleton "c") g)
         assertEqual ".." (1,0,2,100,66.66667) (coupling (Set.fromList ["a","b"]) g)
         assertEqual ".." (0,1,1,0,50) (coupling (Set.fromList ["b","c"]) g)
         assertEqual ".." (1,1,1,50,33.333336) (coupling (Set.fromList ["a","c"]) g)
         assertEqual ".." (0,0,3,0,100) (coupling (ent g) g)
         let sts = Set.fromList $ 
                     Prelude.map Set.fromList [["a","b"],["c"]]
         let rslt = couplings sts g
         let xpct = Map.fromList [
                       (Set.fromList ["a","b"],(1,0,2,100,66.66667)),
                       (Set.fromList ["c"],(0,1,0,0,100)) ]
         assertEqual ".." xpct rslt
    ] 
