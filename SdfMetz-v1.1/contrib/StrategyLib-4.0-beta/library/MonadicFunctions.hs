{-----------------------------------------------------------------------------

	         A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS

		                  StrategyLib

                   Ralf Laemmel                Joost Visser
               CWI & VU, Amsterdam         CWI & SIG, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module defines
auxilliary monadic functions, some of which serve as parametric
polymorphic prototypes for actual strategy combinators.

-----------------------------------------------------------------------------} 

module MonadicFunctions where

import Monad



--- The identity monad -------------------------------------------------------

newtype Id a = Id a

instance Monad Id where
 return = Id
 (Id x) >>= f = f x

unId (Id x) = x



--- Recover from partiality

succeed :: Maybe x -> x
succeed (Just x) = x
succeed Nothing  = error "Didn't succeed!."


 
--- Prototypes for combinators seq, let, choice ------------------------------
 
f `mseq` g    = \x -> f x >>= g           -- monadic variation
f `mlet` g    = \x -> f x >>= \y -> g y x -- a kind of monadic let
f `mchoice` g = \x -> (f x) `mplus` (g x) -- a monadic choice for functions



-- Type guard described by the argument type of a function
argtype     :: MonadPlus m => (x -> y) -> x -> m ()
argtype _ _ =  return ()



-- Type guard described by a type of a value
valtype :: MonadPlus m => x -> x -> m ()
valtype _ _ = return ()



-- A kind of monadic conditional
ifM :: MonadPlus m => m a -> (a -> m c) -> (m c) -> (m c)
ifM ma f mc = ((ma >>= \a -> return (Just a))
               `mplus` (return Nothing)
              ) >>= maybe mc f

------------------------------------------------------------------------------
