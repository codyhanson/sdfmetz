------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module contains functions for manipulating
-- SDF terms.

------------------------------------------------------------------------------

module Language.Sdf.SdfLib
   ( module Language.Sdf.SdfLib
   , module Language.Sdf.Syntax
   , module Language.Sdf.SyntaxPP
   , module Language.Sdf.Pretty
   , module Language.Sdf.EqInstances
   , module Language.Sdf.OrdInstances
   , module Language.Sdf.ShowInstances
   , module Language.Sdf.ReadInstances
   , module Language.Sdf.TermInstances
   , module Language.Sdf.ATermInstances
   ) where 

import Language.Sdf.Syntax
import Language.Sdf.SyntaxPP
import Language.Sdf.Pretty
import Language.Sdf.EqInstances
import Language.Sdf.OrdInstances
import Language.Sdf.ShowInstances
import Language.Sdf.ReadInstances
import Language.Sdf.TermInstances
import Language.Sdf.ATermInstances

import Char(toUpper)
import Control.Monad.Identity (runIdentity)
import StrategyLib
import ATermLib (dehyphen)

------------------------------------------------------------------------------
-- * Collect specific elements from grammars

-- | Collect all lexically defined sorts from an SDF grammar.
collectLexSorts :: SDF -> [Symbol]
collectLexSorts
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where 
      worker (Sdf_lexical_syntax ps) = return (map getSort (getProds ps))
      worker _                       = return []

-- | Collect all defined context-free symbols from an SDF grammar.
collectCfSorts :: SDF -> [Symbol]
collectCfSorts
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where 
      worker (Sdf_context_free_syntax ps) = return (map getSort (getProds ps))
      worker _                            = return []

-- | Collect all context-free productions from an SDF grammar.
collectCfProductions :: SDF -> [Production]
collectCfProductions
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where worker (Sdf_context_free_syntax ps) = return (getProds ps)
          worker _                            = return []

-- | Collect all kernel productions from an SDF grammar.
collectKernelProductions :: SDF -> [Production]
collectKernelProductions
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where worker (Sdf_syntax ps) = return (getProds ps)
          worker _               = return []

-- | Collect all productions from an SDF grammar.
collectProductions :: Term a => a -> [Production]
collectProductions
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where worker (p::Production) = return [p]

-- | Collect all lexical productions from an SDF grammar.
collectLexProductions :: SDF -> [Production]
collectLexProductions
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where worker (Sdf_lexical_syntax ps) = return (getProds ps)
          worker _                       = return []

-- | Collect all lexical and context-free productions from an SDF grammar.
collectLexAndCfProductions :: SDF -> [Production]
collectLexAndCfProductions
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where worker (Sdf_context_free_syntax ps) = return (getProds ps)
          worker (Sdf_lexical_syntax ps)      = return (getProds ps)
          worker _                            = return []

-- | Collect ranges that are non-primitive, i.e. not of the form [c].
collectRanges :: SDF -> [CharRanges]
collectRanges
  = runIdentity . applyTU ( full_tdTU (adhocTU (constTU []) worker) )
    where 
      worker (Sdf_present1 (Sdf_CharRange (Sdf_Character _))) = return []
      worker (Sdf_present1 r) = return [r]
      

-- | Collect all context-free priorities.
collectCfPriorities :: SDF -> [Priority]
collectCfPriorities = runIdentity . applyTU strategy
   where strategy = full_tdTU $ (constTU []) `adhocTU` worker
         worker (Sdf_context_free_priorities (Sdf_comma p)) = return p
         worker _                                           = return []

-- | Collect all lexical priorities.
collectLexPriorities :: SDF -> [Priority]
collectLexPriorities = runIdentity . applyTU strategy
   where strategy = full_tdTU $ (constTU []) `adhocTU` worker
         worker (Sdf_lexical_priorities (Sdf_comma p)) = return p
         worker _                                      = return []

-- | Collect all sorts of an SDF grammar.
collectSortNames :: Term a => a -> [String]
collectSortNames = runIdentity . applyTU strategy
   where strategy = full_tdTU $ (constTU []) `adhocTU` (return . getSortName)


------------------------------------------------------------------------------
-- * Transforming specific SDF parts

-- | Normalize Character Classes
normalizeCharClass :: SDF -> SDF
normalizeCharClass = runIdentity . applyTP ( full_tdTP (adhocTP idTP (return . worker)))
  where 
    worker :: [Symbol] -> [Symbol]
    worker ss = concatMap elimConc ss
    elimConc (Sdf_char_class1 (Sdf_simple_charclass (Sdf_present1 r)))
      = map (\r -> Sdf_char_class1 (Sdf_simple_charclass (Sdf_present1 r))) (listRanges r)
    elimConc s = [s] 
    listRanges (Sdf_conc r1 r2) = listRanges r1 ++ listRanges r2
    listRanges r                = [r]

------------------------------------------------------------------------------
-- * Select subelements from specific grammar elements

-- | Obtain the argument symbols from a production.
getSyms :: Production -> [Symbol]
getSyms (Sdf_prod (Sdf_list7 ss) s as) = ss
getSyms (Sdf_prod_fun l ss s as)       = ss

-- | Obtain the result sort from a production.
getSort :: Production -> Symbol
getSort (Sdf_prod ss s as)       = s
getSort (Sdf_prod_fun l ss s as) = s

-- | Get the sort name of an Sdf_sort or 
getSortName :: Symbol -> [String]
getSortName (Sdf_sort x) = [x]
getSortName _            = []

-- | Obtain the attributes of a production
getAttributes :: Production -> Attributes
getAttributes (Sdf_prod ss s as)       = as
getAttributes (Sdf_prod_fun l ss s as) = as

-- | Obtain the constructor attribute from an attribute list, if it exists.
getConsAttr :: Attributes -> Maybe String
getConsAttr
  = applyTU ( once_tdTU (monoTU getCons) )
    where 
      getCons (Sdf_cons1 (Sdf_fun (Sdf_Literal lit))) 
        = Just . dehyphen . headToUpper . lit2string $ lit
      getCons _				          
        = Nothing
      headToUpper []     = []
      headToUpper (c:cs) = (toUpper c):cs

-- | Get list of productions from 'Productions'.
getProds :: Productions -> [Production]
getProds (Sdf_list8 ps) = ps

------------------------------------------------------------------------------
-- * Check for properties of specific elements

-- | Test whether production has reject or bracket attributes.
isRejectOrBracket :: Production -> Bool
isRejectOrBracket prod
  = case applyTU (once_tdTU (monoTU isRorB)) (getAttributes prod) of
      (Just ()) -> True
      Nothing   -> False
    where isRorB Sdf_reject  = Just ()
          isRorB Sdf_bracket = Just ()
          isRorB _           = Nothing

-- | Test whether production has reject attribute
isReject :: Production -> Bool
isReject prod
  = case applyTU (once_tdTU (monoTU isR)) (getAttributes prod) of
      (Just ()) -> True
      Nothing   -> False
    where isR Sdf_reject  = Just ()
          isR _           = Nothing

------------------------------------------------------------------------------
-- * Conversion helpers

-- | Turn a quoted literal into a non-quoted string
dequote :: String -> String
dequote ('\"':str) 
  = dequote' str
    where 
      dequote' ['\"']         = ""
      dequote' ('\"':['\"'])  = "\""
      dequote' ('\"':cs)      = error $ "Quoted literal extends after end quote: "++cs
      dequote' []             = error $ "Quoted literal misses end quote"
      --dequote' ('\\': c : cs)    = c:(dequote' cs)
      --dequote' ('\\':'\"':cs)    = '\"':(dequote' cs)
      dequote' ('\\':c:cs) = '\\':c:(dequote' cs)   -- just step over any quoted char
      dequote' (c:cs)         = c:(dequote' cs)
	  --dequote' cs = error ("Unexpected: "++cs)
dequote str        
  = error $ "Quoted literal misses starting quote: "++str

-- | Turn a literal (quoted or not) into a (non-quoted) string
lit2string (Sdf_quoted str) = dequote str
lit2string (Sdf_uqlit str)  = str

------------------------------------------------------------------------------
