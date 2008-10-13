-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.FlowGraph.
--
-----------------------------------------------------------------------------

module Language.Sdf.FlowGraphHUnit where

import SGLR
import HUnit
import Debug.HUnitExtras

import Language.Sdf.FlowGraph
import Language.Sdf.SdfLib
import Data.Graph.Components
import Data.Graph.Basics

-----------------------------------------------------------------------------

-- Path of the SDF table required by sglr to parse a grammar
sdfTable = "Language/Sdf/Sdf.tbl"

-- Test grammar: Little Lambda.
llGrammar = "Language/Sdf/Metrics/Strafunski-LittleLambda.def"

-- LittleLambda flow graph.
llGraph :: Gph String
llGraph = mkRel [ ("Decl", "Expr")
                , ("Expr", "Decl")
                , ("Expr", "Expr")
                , ("Expr", "Type")
                , ("Type", "Type")
                ]

main = do
   -- LittleLambda grammar (parsing)
   llAST <- sglr sdfTable llGrammar "SDF" :: IO SDF
   
   -- Run tests
   runTestsTT
      [ testSuite "LittleLambda Test Suite"
           [ TestCase $ assertEqual "graph" llGraph (calcGrammarGraph llAST)
           ]
      ]

-----------------------------------------------------------------------------

