{-----------------------------------------------------------------------------

	          A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS

		                  StrategyLib

                   Ralf Laemmel                Joost Visser
               CWI & VU, Amsterdam          CWI & SIG, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module defines
additional instances of the Monoid class.

-----------------------------------------------------------------------------} 

module MoreMonoids where

import Monoid

instance Num a => Monoid a where
 mempty = 0
 mappend = (+)

-----------------------------------------------------------------------------
