HUnitTest98.lhs  --  test for HUnit, using Haskell language system "98"

$Id: HUnitTest98.lhs,v 1.1 2004/03/01 19:46:31 joost Exp $

> module Main (main) where

> import HUnit
> import HUnitTestBase


> main = runTestTT (test [baseTests])
