-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.Metrics.Halstead.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.HalsteadHUnit where

import HUnit
import Debug.HUnitExtras
import Data.Relation.SetOfPairs

import SGLR
import Language.Sdf.Metrics.Halstead
import Language.Sdf.SdfLib

-----------------------------------------------------------------------------


-- | Path of the SDF table required by sglr to parse a grammar
sdfTable = "Language/Sdf/Sdf.tbl"


main = do
   -- Little Lambda Grammar
   llAST <- sglr sdfTable "Language/Sdf/Metrics/Strafunski-LittleLambda.def" "SDF" :: IO SDF
   let llMetrics = calcHalsteadMetrics llAST
         
   -- Run all the tests
   runTestsTT
     [ testSuite "Little-lambda Test Suite"
          [ TestCase $ assertEqual "Nr Distinct Operators" 3 (u1 llMetrics)
          , TestCase $ assertEqual "Nr Distinct Operands" 13 (u2 llMetrics)
          , TestCase $ assertEqual "Total Nr Operators" 23 (n1 llMetrics)
          , TestCase $ assertEqual "Total Nr Operands" 30 (n2 llMetrics)
          ]
     , testSuite "Math functions"
          [ TestCase $ assertEqual "log2" 1 (log2 2)
          ]
     ]

-----------------------------------------------------------------------------
