-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.Metrics.Ambiguity.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.AmbiguityHUnit where

import HUnit
import Debug.HUnitExtras
import Data.Relation.SetOfPairs

import SGLR
import Language.Sdf.Metrics.Ambiguity
import Language.Sdf.SdfLib

-----------------------------------------------------------------------------


-- | Path of the SDF table required by sglr to parse a grammar
sdfTable = "Language/Sdf/Sdf.tbl"

-----------------------------------------------------------------------------


main = do
   -- Little Lambda Grammar
   llAST <- sglr sdfTable "Language/Sdf/Metrics/Strafunski-LittleLambda.def" "SDF" :: IO SDF
   let llMetrics = calcAmbigMetrics llAST
      
   -- Grammar with productions with attributes
   let attrGrammar = "definition module Assoc exports context-free syntax\n" ++
                      "A \"*\" A -> A { left }\n" ++
                      "Id        -> A\n" ++
                      "ola       -> Id { reject }"
   attrAST <- parseString attrGrammar "SDF" sdfTable :: IO SDF
   let attrMetrics = calcAmbigMetrics attrAST
   
   -- Grammar with context-free priorities
   let priGrammar = "definition module Pri exports\n" ++
                    "context-free priorities\n" ++
                    "      { left:\n" ++
                    "        Expression \"<\"  Expression -> Expression\n" ++
                    "        Expression \"<\"  Expression -> Expression\n" ++
                    "        Expression \"<=\" Expression -> Expression\n" ++
                    "      } >\n" ++
                    "      \"not\" Expression -> Expression >\n" ++
                    "      Expression \"and\" Expression -> Expression\n" ++
                    "context-free priorities\n" ++
                    "      \"not\" Expression -> Expression >\n" ++
                    "      \"not\" Expression -> Expression >\n" ++
                    "      Expression \"<=>\" Expression -> Expression\n"
   priAST <- parseString priGrammar "SDF" sdfTable :: IO SDF
   let priMetrics = calcAmbigMetrics priAST

   -- Run all the tests
   runTestsTT
     [ testSuite "Little-lambda Test Suite"
          [ TestCase $ assertEqual "calcFollowRestr" 2 (frst llMetrics)
          , TestCase $ assertEqual "calcAssocAttr"   0 (assoc llMetrics)
          ]
     , testSuite "Grammar of Productions with Attributes Test"
          [ TestCase $ assertEqual "calcAssocAttr" 1 (assoc attrMetrics)
          , TestCase $ assertEqual "calcRejectProds" 1 (rejP attrMetrics)
          ]
     , testSuite "Grammar with context-free priorities"
          [ TestCase $ assertEqual "calcUPPri" 5 (uppri priMetrics)
          , TestCase $ assertEqual "calcTNUPEG" 7 (tnupeg priMetrics)
          , TestCase $ assertEqual "calcTNPC"  2 (tnpc priMetrics)
          , TestCase $ assertEqual "calcTNG"   6 (tng priMetrics)
          ] 
     ]

-----------------------------------------------------------------------------

