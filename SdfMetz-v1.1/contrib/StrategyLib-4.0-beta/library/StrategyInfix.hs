{-----------------------------------------------------------------------------

	        A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS
				   
 		                StrategyLib

                   Ralf Laemmel                Joost Visser
               CWI & VU, Amsterdam         CWI & SIG, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module indicates
how some strategy combinators could be denoted via infix combinators.

-----------------------------------------------------------------------------}

module StrategyInfix where

import StrategyPrelude
import OverloadingTheme

infixl 1  >>>, >>>=, >>>-
infixl 2  -+

s >>> s'	= s `seqS` s'
s >>>= s'	= s `passS` s'
s >>>- s'	= s `passS` \_ -> s'
s -+ f          = s `adhocS` f

{-

tst :: TP Maybe
tst = idTP >>> failS -+ f -+ f

f (x::Char) = return x

mytest :: Maybe Char
mytest = applyTP tst 'a'

mytest2 :: Maybe Bool 
mytest2 = applyTP tst True

-}
