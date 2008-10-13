      
module Data.Relation.FunctionalDependenciesHUnit where

import Data.Relation.FunctionalDependencies
import Data.Relation.SetOfPairs
import Data.Set as Set
import HUnit

-----------------------------------------------------------------------------

main
  = runTestTT . TestList $ testList
  
labeledTest l t
  = TestLabel l (TestCase t)

testList
  = [labeledTest "satisfaction of functional dependency (1)" $ do
         let r = inv $ mkRel [(2,-1),(5,-1),(17,1),(10,-2),(4,-2),(1,2)]
         let fd = mkFunDep (^2) (`rem` 3) r
         assertBool "yes" ( satisfiesFunDep fd r )
      ,
      labeledTest "satisfaction of functional dependency (2)" $ do
         let r = inv $ mkRel [((1,2),-1),((1,10),-1),((0,0),1),((5,6),-2),((5,0),-2),((1,2),2)]
         let fd = mkFunDep id fst r
         assertBool "yes" ( satisfiesFunDep fd r )
      ,
      labeledTest "satisfaction of functional dependency (3)" $ do
         let r = inv $ mkRel [((1,2),-1),((1,10),-1),((0,0),1),((5,6),-2),((5,0),-2),((1,2),2),
                              ((0,0),2)]
         let fd = mkFunDep id fst r
         assertBool "no" ( not $ satisfiesFunDep fd r )
      ,
      labeledTest "satisfaction of functional dependency for R=bottom and f=id g=id" $ do
         let r = emptyRel:: Rel Int Int 
         let fd = mkFunDep id id r
         assertBool "yes" ( satisfiesFunDep fd r )
      ,
      labeledTest "satisfaction of functional dependency f=id g=!" $ do
         let r = inv $ mkRel [(2,-1),(5,-1),(17,1),(10,-2),(4,-2),(1,2)] 
         let fd = mkFunDep id (const ()) r
         assertBool "yes" ( satisfiesFunDep fd r )
      ,
      labeledTest "satisfaction of functional dependency f=id g=!" $ do
         let r = inv $ mkRel [(2,-1),(5,-1),(17,1),(10,-2),(4,-2),(1,2)] 
         let fd = mkFunDep id (const ()) r
         assertBool "yes" ( satisfiesFunDep fd r )
      ,
      labeledTest "projection of an n-ary relation" $ do
         let r = setRel $ Set.fromList $ [(33234, "Matemática", 17),(33123, "Algebra", 12),(44312, "Análise", 14)]
         let p = proj (mkRel [(a, (\(x,y,z) -> x) a) | (a,_) <- pairs r]) r
         assertBool "yes" ( p == Set.fromList [(33234, 33234),(33123, 33123),(44312, 44312)])]



hotel = setRel $ Set.fromList  [   
            ("1","100","1","2","50"),
            ("4","101","1","2","50"),           
            ("1","200","1","2","50"),
            ("2","200","1","3","70"),
            ("1","102","2","2","70"),            
            ("2","101","3","3","100"),
            ("3","100","3","2","50")
                 ]

hotel2 = setRel $ Set.fromList  [   
            ("1","100","1","2","50"),
            ("4","101","1","2","50"),           
            ("1","200","1","2","50"),
            ("2","200","1","3","70"),
            ("2","102","2","2","70"),            
            ("2","101","3","3","100"),
            ("3","100","3","2","50")
                 ]


-- A -> D
adant = \(a, _, _, _, _) -> a 

adcons = \(_, _, _, d, _) -> d


funDep = FunDep {antecedent = setRel $ Set.fromList [(1,1),(4,4),(1,1),(2,2),(1,1),(2,2),(3,3)],
                 consequent = setRel $ Set.fromList [(2,2),(2,2),(2,2),(3,3),(2,2),(2,2),(2,2)]}

-- True
testFD = let x = mkFunDep adant adcons hotel
         in satisfiesFunDep'' x hotel2
