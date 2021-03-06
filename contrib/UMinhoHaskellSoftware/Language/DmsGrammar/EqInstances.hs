module Language.DmsGrammar.EqInstances where

import Language.DmsGrammar.Syntax

{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Syntax.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Eq DmsSyntax where
    (DmsSyntax aa) == (DmsSyntax aa') = aa == aa'
    _ == _ = False

instance Eq DmsRule where
    (Other aa ab) == (Other aa' ab') = aa == aa' && ab == ab'
    (Rule aa ab ac) == (Rule aa' ab' ac') = aa == aa' && ab == ab' &&
		    ac == ac'
    (Disambiguation aa) == (Disambiguation aa') = aa == aa'
    _ == _ = False

instance Eq DmsSymbol where
    (DmsNonTerminal aa) == (DmsNonTerminal aa') = aa == aa'
    (DmsTerminal aa) == (DmsTerminal aa') = aa == aa'
    _ == _ = False

--  Imported from other files :-
