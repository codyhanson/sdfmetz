-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.Metrics.Structure.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.StructureHUnit where

import SGLR
import HUnit
import Debug.HUnitExtras

import Language.Sdf.Metrics.Structure
import Language.Sdf.SdfLib
import Data.Relation.SetOfPairs

-----------------------------------------------------------------------------

-- Path of the SDF table required by sglr to parse a grammar
sdfTable = "Language/Sdf/Sdf.tbl"

-- Test grammar
testGrammar = "Language/Sdf/Metrics/Strafunski-LittleLambda.def"

main = do
   -- LittleLambda grammar (parsing and metrics calculation)
   llAST <- sglr sdfTable testGrammar "SDF" :: IO SDF
   let llMetrics = calcStructMetrics llAST
   
   -- Run metrics
   runTestsTT
      [ testSuite "LittleLambda Test Suite"
           [ TestCase $ assertEqual "timp" 0 (timp  llMetrics)
           , TestCase $ assertEqual "lev" 2 (lev llMetrics)
           , TestCase $ assertEqual "clev" ((2/3)*100) (clev llMetrics)
           , TestCase $ assertEqual "nslev" 1 (nslev llMetrics)
           , TestCase $ assertEqual "dep" 2 (dep llMetrics)
           , TestCase $ assertEqual "hei" 2 (hei llMetrics)
           ]
      ]

-----------------------------------------------------------------------------

