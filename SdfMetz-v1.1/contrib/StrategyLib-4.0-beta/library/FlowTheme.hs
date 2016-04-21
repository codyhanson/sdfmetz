{-----------------------------------------------------------------------------

	          A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS

		                  StrategyLib

                   Ralf Laemmel                Joost Visser
               CWI & VU, Amsterdam          CWI & SIG, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module defines
combinators to wire up control and data flow. Whenever possible,
we define the combinators in an overloaded fashion but we provide
type-specialised variants for TP and TU for convenience.

-----------------------------------------------------------------------------} 



module FlowTheme where

import StrategyPrelude
import OverloadingTheme
import Monad
import Monoid



--- Always succeed by a catch-all, say recover from failure ------------------

tryS         :: (StrategyPlus s m, StrategyMonoid s m) => s m -> s m
tryS s       =  s `choiceS` skipS

tryTP        :: MonadPlus m => TP m -> TP m
tryTP        =  tryS

tryTU        :: (MonadPlus m, Monoid u) => TU u m -> TU u m
tryTU  	     =  tryS



--- Test for a strategy's success in a type-preserving context ---------------

testS           :: Strategy s m => s m -> TP m
testS s         =  voidS s `passS` const idTP

testTP 		:: Monad m => TP m -> TP m
testTP  	=  testS

testTU 		:: Monad m => TU a m -> TP m
testTU  	=  testS



--- If-then-else with value passing ------------------------------------------

ifS       :: StrategyPlus s m => TU u m -> (u -> s m) -> s m -> s m
ifS c t e =  ((c `passTU` (constTU . Just)) `choiceTU` constTU Nothing)
             `passS`
             maybe e t

ifTP      :: MonadPlus m => TU u m -> (u -> TP m) -> TP m -> TP m
ifTP      =  ifS

ifTU      :: MonadPlus m => TU u m -> (u -> TU u' m) -> TU u' m -> TU u' m
ifTU      =  ifS



--- Disciplined form of a guarding -------------------------------------------

ifthenS     :: Strategy s m => TU () m -> s m -> s m
ifthenS c t =  c `passS` const t

ifthenTP    :: Monad m => TU () m -> TP m -> TP m
ifthenTP    =  ifthenS

ifthenTU    :: Monad m => TU () m -> TU u m -> TU u m
ifthenTU    =  ifthenS



--- Negation by failure ------------------------------------------------------

notS    :: StrategyPlus s m => s m -> TP m
notS s  =  ifS (voidS s) (const failTP) idTP

notTP   :: MonadPlus m => TP m -> TP m
notTP   =  notS

notTU   :: MonadPlus m => TU u m -> TP m
notTU   = notS



--- Exclusive choice ---------------------------------------------------------

xchoiceS        :: StrategyPlus s m => s m -> s m -> s m
s `xchoiceS` s' =  (notS s' `seqS` s) `choiceS` (notS s `seqS` s')

xchoiceTP       :: MonadPlus m => TP m -> TP m -> TP m
xchoiceTP       =  choiceS

xchoiceTU       :: MonadPlus m => TU u m -> TU u m -> TU u m
xchoiceTU       =  choiceS



--- Generic filters derived from monomorphic predicates ----------------------

filterTP        :: (Term t, MonadPlus m) => (t -> Bool) -> TP m
filterTP g      =  monoTP (\x -> if g x then return x else mzero)

filterTU        :: (Term t, MonadPlus m) => (t -> Bool) -> TU t m
filterTU g      =  monoTU (\x -> if g x then return x else mzero)



--- Generic tickers derived from monomorphic predicates  ---------------------

tickTU 	        :: (Monad m, Term t, Num n) => (t -> Bool) -> TU n m
tickTU g        =  adhocTU (constTU 0) (\t -> return (if g t then 1 else 0))



--- Type guards, i.e., guards that do not observe values ---------------------
--- Typical usage: full_tdTU (typeTickTU (typeGuard::TypeGuard MyType)) ------

type TypeGuard a =  a -> ()

typeGuard	 :: TypeGuard a
typeGuard	 =  const ()



--- Generic tickers derived from type guards ---------------------------------

typeTickTU  	 :: (Term t, Monad m, Num n) => TypeGuard t -> TU n m
typeTickTU g	 =  adhocTU (constTU 0) ((\() -> return 1) . g)



--- Generic filters derived from type guards ---------------------------------

typeFilterTP     :: (Term t, MonadPlus m) => TypeGuard t -> TP m
typeFilterTP g   =  monoTP (\x -> ((\() -> return x) . g) x)

typeFilterTU     :: (Term t, MonadPlus m) => TypeGuard t -> TU t m
typeFilterTU g   =  monoTU (\x -> ((\() -> return x) . g) x)



------------------------------------------------------------------------------
