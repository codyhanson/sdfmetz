-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.Metrics.NPath.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.NPathHUnit where

import HUnit
import Debug.HUnitExtras
import Data.Map as Map

import SGLR
import Language.Sdf.Metrics.NPath
import Language.Sdf.SdfLib

-----------------------------------------------------------------------------


-- | Path of the SDF table required by sglr to parse a grammar
sdfTable = "Language/Sdf/Sdf.tbl"


-- * Set o test examples

sdfHeader :: String
sdfHeader = "definition module Assoc exports context-free syntax"

ex1 :: String
ex1 = unlines
   [ sdfHeader
   , "   \"@\" Identifier -> TypeVariableIdentifier { cons(\"TypeVarId\") }"
   ]

ex2 :: String
ex2 = unlines
   [ sdfHeader
   , "   Type | (\"(\" \")\") -> DiscretionaryType { cons(\"DiscretionaryType\") }"
   ]

ex3 :: String
ex3 = unlines
   [ sdfHeader
   , "   \"state\" Identifier \"of\" FieldList Invariant? Initialization? \"end\" -> StateDefinition { cons(\"StateDef\") }"
   ]

ex4 :: String
ex4 = unlines
   [ sdfHeader
   , "   \"values\" { ValueDefinition \";\" }+ -> ValueDefinitions { cons(\"ValueDefinitions\") }"
   ]

ex5 :: String
ex5 = unlines
   [ sdfHeader
   , "   Parameters+ -> ParametersList { cons(\"ParametersList\") }"
   ]

ex6 :: String
ex6 = unlines
   [ sdfHeader
   , "   \"if\" Expression \"then\" Expression ElseIfExpression* \"else\" Expression -> Expression"
   ]

ex7 :: String
ex7 = unlines
   [ sdfHeader
   , "   \"while\" Expression \"do\" Statement -> Statement { cons(\"WhileLoop\") }"
   , "   \"||\" \"(\" { Statement \",\" }+ \")\" -> Statement { cons(\"NonDetStmt\") }"
   , "   CallStatement -> Statement { cons(\"CallStmt\") }"
   ]

ex8 :: String
ex8 = unlines
   [ sdfHeader
   , "   \"let\" { LocalDefinition \",\" }+ \"in\" Statement -> Statement { cons(\"LetStmt\") }"
   , "   \"let\" Bind ( \"be\" \"st\" Expression )? \"in\" Statement -> Statement { cons(\"LetBeSTStmt\") }"
   , "   FunctionDefinition | ValueDefinition -> LocalDefinition { cons(\"LocalDefinition\") }"   
   ]
   
   

-- * Main function

main = do
   (ex1AST, ex1Metrics) <- processExString ex1
   (ex2AST, ex2Metrics) <- processExString ex2
   (ex3AST, ex3Metrics) <- processExString ex3
   (ex4AST, ex4Metrics) <- processExString ex4
   (ex5AST, ex5Metrics) <- processExString ex5
   (ex6AST, ex6Metrics) <- processExString ex6
   (ex7AST, ex7Metrics) <- processExString ex7
   (ex8AST, ex8Metrics) <- processExString ex8

   -- Run all the tests
   runTestsTT
     [ testSuite "Test complete example's metrics"
          [ TestCase $ assertEqual "Example 1: AVG NPATHp" (npathp ex1Metrics) 1
          , TestCase $ assertEqual "Example 1: AVG NPATHn" (npathn ex1Metrics) 1
          , TestCase $ assertEqual "Example 1: MAX NPATHp" (maxnpathp ex1Metrics) 1
          , TestCase $ assertEqual "Example 1: MAX NPATHn" (maxnpathn ex1Metrics) 1
          , TestCase $ assertEqual "Example 7: AVG NPATHp" (npathp ex7Metrics) 1.3333334
          , TestCase $ assertEqual "Example 7: AVG NPATHn" (npathn ex7Metrics) 4
          , TestCase $ assertEqual "Example 7: MAX NPATHp" (maxnpathp ex7Metrics) 2
          , TestCase $ assertEqual "Example 7: MAX NPATHn" (maxnpathn ex7Metrics) 4
          , TestCase $ assertEqual "Example 8: AVG NPATHp" (npathp ex8Metrics) 2
          , TestCase $ assertEqual "Example 8: AVG NPATHn" (npathn ex8Metrics) 3
          , TestCase $ assertEqual "Example 8: MAX NPATHp" (maxnpathp ex8Metrics) 2
          , TestCase $ assertEqual "Example 8: MAX NPATHn" (maxnpathn ex8Metrics) 4
          ]
     , testSuite "Simple examples"
          [ testCalcNPath "ex1: Simple sequence" ex1AST [("TypeVariableIdentifier", [1])]
          , testCalcNPath "ex2: Alternative operator" ex2AST [("DiscretionaryType", [2])]
          , testCalcNPath "ex3: Two optional operators" ex3AST [("StateDefinition", [4])]
          , testCalcNPath "ex4: Simple + iterator w/ separator" ex4AST [("ValueDefinitions", [2])]
          , testCalcNPath "ex5: Simple + iterator" ex5AST [("ParametersList", [1])]
          , testCalcNPath "ex6: Simple * iterator" ex6AST [("Expression", [2])]
          ]
     , testSuite "Examples with multiple productions"
          [ testCalcNPath "ex7: Three simple rules" ex7AST [("Statement", [1,2,1])]
          , testCalcNPath "ex8: Three rules w/ multiple paths" ex8AST [("LocalDefinition", [2]), ("Statement", [2,2])]
          ]     
     , testSuite "Auxiliary functions"
          [ TestCase $ assertEqual "addMetric" [("a",[3])] (toList $ addMetric empty "a" 3)
          , TestCase $ assertEqual "addMetric" [("a",[3,3])] (toList $ addMetric (fromList [("a",[3])]) "a" 3)
          , TestCase $ assertEqual "addMetric" [("a",[3]),("b",[4])] (toList $ addMetric (fromList [("a",[3])]) "b" 4)
          ]
     ]

-- * Auxiliar functions

-- | Auxiliar function that parses a string a returns a pair with the 
--   corresponding AST and the computed metrics over it.
processExString :: String -> IO ( SDF, NPathMetrics)
processExString str = do
   ast <- parseString str "SDF" sdfTable :: IO SDF
   return (ast, calcNPathMetrics ast)


-- | Auxiliar function that encapsulates 
testCalcNPath :: String -> SDF -> [(String,[Int])] -> Test
testCalcNPath msg ex values = TestCase $ assertEqual msg values (toList $ calcNPath ex)


-----------------------------------------------------------------------------
