-----------------------------------------------------------------------------
-- |
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Unit tests for Language.Sdf.SdfLib.
--
-----------------------------------------------------------------------------

module Language.Sdf.SdfLibHUnit where

import HUnit
import Debug.HUnitExtras

import Language.Sdf.SdfLib

-----------------------------------------------------------------------------

main = runTestsTT [
  labeledTest "dequote"  $ do
    assertEqual "alpha" ['a'] $ dequote ['"','a','"']
    assertEqual "symbol" ['+'] $ dequote ['"','+','"']
    assertEqual "escape" ['\\','n'] $ dequote ['"','\\','n','"']
    assertEqual "escaped escape" ['\\','\\'] $ dequote ['"','\\','\\','"']
    assertEqual "escaped quote" ['\\','"'] $ dequote ['"','\\','"','"']
 ]

-----------------------------------------------------------------------------
