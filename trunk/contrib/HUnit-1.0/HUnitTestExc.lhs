HUnitTestExc.lhs  --  test for HUnit, using Haskell language system "Exc"

$Id: HUnitTestExc.lhs,v 1.1 2004/03/01 19:46:31 joost Exp $

> module Main (main) where

> import HUnit
> import HUnitTestBase
> import qualified Exception (assert)


 assertionMessage = "HUnitTestExc.lhs:13: Assertion failed\n"
 assertion = Exception.assert False (return ())



> main = runTestTT (test [baseTests, excTests])

> excTests = test [

    -- Hugs and GHC don't currently catch arithmetic exceptions.
   "div by 0" ~:
     expectUnspecifiedError (TestCase ((3 `div` 0) `seq` return ())),

    -- GHC doesn't currently catch array-related exceptions.
   "array ref out of bounds" ~:
     expectUnspecifiedError (TestCase (... `seq` return ())),

>   "error" ~:
>     expectError "error" (TestCase (error "error")),

>   "tail []" ~:
>     expectUnspecifiedError (TestCase (tail [] `seq` return ()))

   -- Hugs doesn't provide `assert`.
   "assert" ~:
     expectError assertionMessage (TestCase assertion)

>  ]
