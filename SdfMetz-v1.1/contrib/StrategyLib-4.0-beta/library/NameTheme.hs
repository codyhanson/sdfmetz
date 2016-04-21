{-----------------------------------------------------------------------------

	         A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS

		                  StrategyLib

                   Ralf Laemmel                Joost Visser
               CWI & VU, Amsterdam            CWI, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module provides
algorithms to collect names and their types.

-----------------------------------------------------------------------------} 

module NameTheme where

import Monad
import List
import StrategyPrelude
import OverloadingTheme
import FlowTheme
import TraversalTheme
import MonadIdentity hiding (fail)


--- Generic free name analysis algorithm --------------------------------------

freeNames :: (Eq name, Term t)
          => TU [(name,tpe)] Identity	-- Identify declarations
          -> TU [name] Identity		-- Identify references
          -> t				-- Input term
          -> [name]			-- Free names
freeNames declared referenced =
   runIdentity .
   applyTU (all_recTU (op2TU combine)
                      (op2TU (,) declared referenced))
  where
   combine (decs,refs) recs =
    (refs `union` recs) \\ (map fst decs)



--- Accumulate declarations for focus -----------------------------------------

boundTypedNames :: (Term f, Term t, Eq name)
                => TU [(name,tpe)] Identity	-- Identify declarations
                -> (f -> Maybe f)		-- Get focus
                -> t				-- Input term
                -> Maybe ([(name,tpe)],f)	-- Derived declarations
boundTypedNames declared unwrap =
   applyTU (once_pe (adhocTU failTU . stop) bind [])
  where
    stop inh =
      (maybe Nothing (Just . (,) inh)) .
      unwrap
    bind inh =
      msubstTU (Just . runIdentity) declared `passTU` \decs ->
      constTU (unionBy byName decs inh)
    byName = \a -> \a' -> fst a == fst a'



--- Generic free name analysis algorithm with types ---------------------------
freeTypedNames :: (Eq name, Term t)
               => TU [(name,tpe)] Identity -- Identify declarations
               -> TU [name] Identity	   -- Identify references
               -> [(name,tpe)]		   -- Derived declarations
               -> t			   -- Input term
               -> [(name,tpe)]	   	   -- Free names with types
freeTypedNames declared referenced types t =
   filter (\e -> elem (fst e) names) types
  where
    names = freeNames declared referenced t


------------------------------------------------------------------------------ 
