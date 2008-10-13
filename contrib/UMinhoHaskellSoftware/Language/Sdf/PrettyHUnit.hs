-----------------------------------------------------------------------------
-- |
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.Pretty.
--
-----------------------------------------------------------------------------

module Language.Sdf.PrettyHUnit where

import HUnit
import Debug.HUnitExtras

import Language.Sdf.Syntax
import Language.Sdf.Pretty
import Language.Sdf.ATermInstances
import Language.Sdf.EqInstances
import Language.Sdf.ShowInstances

import System.IO
import SGLR
import ATermLib
import StrategyLib

-----------------------------------------------------------------------------

main = runTestsTT [
  labeledTest "No spaces in symbols"  $ do
     assertEqual "sort" "S" (renderSdf $ Sdf_sort $ "S")
     assertEqual "opt" "S?" (renderSdf $ Sdf_opt $ Sdf_sort $ "S")
     assertEqual "opt" "S+" (renderSdf $ Sdf_iter $ Sdf_sort $ "S")
     assertEqual "opt" "S*" (renderSdf $ Sdf_iter_star $ Sdf_sort $ "S")
     assertEqual "opt" "{S T}+" (renderSdf $ Sdf_iter_sep (Sdf_sort "S") (Sdf_sort "T"))
     assertEqual "opt" "{S T}*" (renderSdf $ Sdf_iter_star_sep (Sdf_sort "S") (Sdf_sort "T"))
     assertEqual "opt" "<S-CF>" (renderSdf $ Sdf_cf $ Sdf_sort $ "S")
     assertEqual "opt" "<S-LEX>" (renderSdf $ Sdf_lex $ Sdf_sort $ "S")
  ,
  labeledTest "parse-render-parse" $ 
    parseRenderParse (ph::Ph Symbol) "S*+?" "Symbol"
  ,
  labeledTest "Several symbols" $ 
    parseRenderParse (ph::Ph Symbol) "( S? S+ S* {S s}* )" "Symbol"
  ,
  labeledTest "Tricky quoted things" $ 
    parseRenderParse (ph::Ph Symbol) "( A \"A\" \"\\\"\" )" "Symbol"   -- ( A "A" "\"" )
 ]

  
parseRenderParse :: (ATermConvertible a, Term a, Show a, Eq a) 
                 => Ph a -> String -> String -> Assertion 
parseRenderParse (ph::Ph a) string startSymbol = do
  let str1 = string
  putStrLn ""
  (ast1::a) <- parseString str1 startSymbol sdfTable
  let str2 = renderSdf ast1
  (ast2::a) <- parseString str2 startSymbol sdfTable
  assertEqual "AST" ast1 ast2
  assertEqual "Sentence" str1 str2
 

newtype Ph a = Ph ()
ph :: Ph a
ph = Ph ()

sdfTable :: String
sdfTable = "Language/Sdf/Sdf.tbl"

-----------------------------------------------------------------------------
