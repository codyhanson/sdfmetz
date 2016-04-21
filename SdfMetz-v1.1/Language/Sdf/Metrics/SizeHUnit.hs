-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.Metrics.Size.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.SizeHUnit where

import HUnit
import Debug.HUnitExtras
import Data.Relation.SetOfPairs

import SGLR
import Language.Sdf.Metrics.Size
import Language.Sdf.SdfLib


-----------------------------------------------------------------------------

-- | Path of the SDF table required by sglr to parse a grammar
sdfTable = "Language/Sdf/Sdf.tbl"

-----------------------------------------------------------------------------

main = do
   -- Little Lambda Grammar
   llAST <- sglr sdfTable "Language/Sdf/Metrics/Strafunski-LittleLambda.def" "SDF" :: IO SDF
   let llMetrics = calcSizeMetrics llAST
      
   -- Dummy Grammar with single reject rule
   let dgrGrammar = "definition module Rej exports context-free syntax A -> B { reject }"
   dgrAST <- parseString dgrGrammar "SDF" sdfTable :: IO SDF
   let dgrMetrics = calcSizeMetrics dgrAST
   
   -- Part of the ISO VDM version 1.25. This rule is recognized by Sdf as
   -- Sdf_prod_fun and causes some problems in the metrics.
   let g125 = "definition module Lol exports context-free syntax " ++
              "\"is_\" ( \"bool\" | \"nat\" | \"nat1\" | \"int\" | \"rat\" | \"real\" | \"char\" | \"token\" ) -> IsBasicType"
   g125AST <- parseString g125 "SDF" sdfTable :: IO SDF
   let g125Prods = (filter (not . isReject)) . collectCfProductions $ g125AST

   -- Run all the tests
   runTestsTT
     [ testSuite "Empty Grammar Test Suite"
          [ TestCase $ assertEqual "calcVAR"  0 (calcVAR [])
          , TestCase $ assertEqual "calcMCC"  0 (calcMCC [])
          , TestCase $ assertEqual "calcAVS"  0 (calcAvsN 0 0)
          ]
     , testSuite "Little-lambda Test Suite"
          [ TestCase $ assertEqual "calcTERM" 10 (term llMetrics)
          , TestCase $ assertEqual "calcVAR"  3 (var llMetrics)
          , TestCase $ assertEqual "calcUVAR" 4 (uvar llMetrics)
          , TestCase $ assertEqual "calcMCC"  5 (mcc llMetrics)
          , TestCase $ assertEqual "calcAvsN"  (23/3) (avsN llMetrics)
          , TestCase $ assertEqual "calcAvsP" (23/7) (avsP llMetrics)
          ]
     , testSuite "Rejects Test Suite"
          [ TestCase $ assertEqual "calcMCC" 0 (mcc dgrMetrics)
          , TestCase $ assertEqual "calcAVS" 0 (avsN dgrMetrics)
          , TestCase $ assertEqual "calcTERM" 0 (term dgrMetrics)
          , TestCase $ assertEqual "calcVAR" 0 (var dgrMetrics)
          ]
     , testSuite "Odd cases"
          [ TestCase $ assertEqual "calcTERM" 9 (calcTERM g125AST)
          ]
     ]

-----------------------------------------------------------------------------
