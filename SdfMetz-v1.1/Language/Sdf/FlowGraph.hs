-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Functionality regarding the flow graph of an SDF grammar.
--
-----------------------------------------------------------------------------

module Language.Sdf.FlowGraph where

import StrategyLib
import Language.Sdf.SdfLib
import Data.Relation.SetOfPairs
import Control.Monad.Identity (runIdentity)

-----------------------------------------------------------------------------


-- | Calculate and print the elements of the non-singleton components of the
--   grammar.
calcNonSingletonLevels :: SDF -> [[String]]
calcNonSingletonLevels = map setToList . nslevels
   where levels   = strongNonSingletonComponentSet . calcGrammarGraph
         nslevels = filter ((> 1) . cardinality) . setToList . levels


-----------------------------------------------------------------------------
-- * Helper functions

-- | Calculates the "control flow" graph of a Grammar where the node are the
--   non-terminals names (strings) and the edges from node "a" to other nodes
--   are created if the other nodes are non-terminals that appears in the RHS
--   of the non-terminal "a".
calcGrammarGraph :: SDF -> Gph String
calcGrammarGraph sdf =
   let prods    = (filter (not . isReject) . collectCfProductions) sdf
       defSorts = concatMap (getSortName . getSort) prods
       pairs    = concatMap ruleDeps prods
   in mkRel [ (x,y) | (x,y) <- pairs, y `elem` defSorts ]
   
   
-- | Calculates all dependencies of a rule by creating a list of pairs which
--   the first element represent the non-terminal of the rule and the second
--   represents the dependent non-terminal.
ruleDeps :: Production -> [(String,String)]
ruleDeps p = 
   let sortname = getSortName $ getSort p
       symbols  = (runIdentity . applyTU strategy . getSyms) p
       strategy = full_tdTU $ (constTU []) `adhocTU` (return . getSortName)
   in [(a,b) | a <- sortname, b <- symbols]
