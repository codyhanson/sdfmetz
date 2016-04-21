{-----------------------------------------------------------------------------

	        A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS

		                StrategyLib

                  Ralf Laemmel                Joost Visser
              CWI & VU, Amsterdam         CWI & SIG, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module overloads
basic combinators to enable uniform treatment of TU and TP strategies.
The overloading scheme is motivated in the "... Polymorphic Symphony"
paper. The names in the present module deviate from the paper in that
the are postfixed by an "...S" in order to rule out name clashes and
to avoid labour-intensive resolution. The class constraints in this
module seem to be outrageous but this has to do with a type
inferencing bug for class hierarchies in hugs. This bug is removed in
the October 2002 release.

-----------------------------------------------------------------------------} 

module OverloadingTheme where

import Monad
import Monoid
import StrategyPrelude


--- Overload completely unconstrained strategy combinators -------------------

class Monad m => Strategy s m
  where
    voidS :: s m -> TU () m
    seqS  :: TP m -> s m -> s m
    passS :: TU a m -> (a -> s m) -> s m
  
instance Monad m => Strategy TP m
  where
    voidS = voidTP
    seqS  = seqTP
    passS = passTP

instance Monad m => Strategy (TU a) m
  where
    voidS = voidTU
    seqS  = seqTU
    passS = passTU


--- Overload basic combinators which might involve a monoid  -----------------

class (Monad m, Strategy s m) => StrategyMonoid s m
  where
    skipS    :: s m
    allS     :: s m -> s m
    combS    :: s m -> s m -> s m

instance (Monad m, Strategy TP m) => StrategyMonoid TP m
  where
    skipS    = idTP
    allS     = allTP
    combS    = seqTP

instance (Monad m, Monoid u, Strategy (TU u) m) => StrategyMonoid (TU u) m
  where
    skipS    = constTU mempty
    allS     = allTU'
    combS    = op2TU mappend



--- Overload basic combinators which involve MonadPlus -----------------------

class (Strategy s m, Monad m, MonadPlus m) => StrategyPlus s m
  where
    failS   :: s m
    choiceS :: s m -> s m -> s m
    oneS    :: s m -> s m

instance (Monad m, MonadPlus m, Strategy TP m) => StrategyPlus TP m
  where
    failS   = failTP
    choiceS = choiceTP
    oneS    = oneTP



instance (Monad m, MonadPlus m, Strategy (TU u) m) => StrategyPlus (TU u) m
  where
    failS   = failTU
    choiceS = choiceTU
    oneS    = oneTU



--- Overload apply and adhoc combinators -------------------------------------

class (Strategy s m, Monad m, Term t) => StrategyApply s m t x | s t -> x
  where
    applyS :: s m -> t -> m x
    adhocS :: s m -> (t -> m x) -> s m

instance (Monad m, Term t) => StrategyApply TP m t t
  where
    applyS = applyTP
    adhocS = adhocTP

instance (Monad m, Term t) => StrategyApply (TU a) m t a
  where
    applyS = applyTU
    adhocS = adhocTU



--- Overloaded lifting with failure ------------------------------------------

monoS f = adhocS failS f



--- Overload msubst combinator (Experimental) --------------------------------

class StrategyMSubst s
  where
    msubstS :: (Monad m, Monad m') => (forall t . m t -> m' t) -> s m -> s m'
    
instance StrategyMSubst TP
  where
    msubstS f = msubstTP f
    
instance StrategyMSubst (TU a)
  where
    msubstS f = msubstTU f    



------------------------------------------------------------------------------
