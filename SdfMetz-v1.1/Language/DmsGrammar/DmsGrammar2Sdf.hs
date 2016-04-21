------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Tiago Alves
-- Stability	: experimental
-- Portability	: portable
--
-- This module contains the convertion between DMS Grammars to SDF grammars.

------------------------------------------------------------------------------

module Language.DmsGrammar.DmsGrammar2Sdf (dmsGrammar2Sdf) where

import Language.DmsGrammar.Syntax
import Language.Sdf.Syntax
import Monad


-- | Converts a DMS Grammar AST to an SDF Grammar AST
dmsGrammar2Sdf :: DmsSyntax -> SDF
dmsGrammar2Sdf (DmsSyntax rules)
   = sdfHeader $ rules >>= convertRule
     

-- | Creates a generic header for an SDF grammar
sdfHeader :: [Production] -> SDF
sdfHeader x = Sdf_definition $ Sdf_list4 [sdfmodule]
   where sdfmodule = Sdf_module_ (Sdf_unparameterized "main") [] (Sdf_list5 [section])
         section = Sdf_exports_ $ Sdf_context_free_syntax $ Sdf_list8 x
         

-- | Converts a DMS grammar rule to an SDF grammar rule. It uses MonadPlus
--   to deal with partiality because we only want to convert one specific
--   rule of the Dms Grammar and ignore the others.
convertRule :: MonadPlus m => DmsRule -> m Production
convertRule (Rule nt lst _)
    = return $ Sdf_prod (Sdf_list7 (rhs)) (lhs) Sdf_no_attrs
      where lhs = Sdf_sort nt
            rhs = map convertSymbol lst
convertRule _ = mzero


-- | Convert a DMS Grammar Symbol (terminar or non-terminal) to its equivalent
--   in SDF notation.
convertSymbol :: DmsSymbol -> Symbol
convertSymbol (DmsNonTerminal nt) = Sdf_sort nt
convertSymbol (DmsTerminal t)     = Sdf_lit $ Sdf_quoted (convertQuotes t)


-- | Convert terminal DMS grammar convention which uses single quotes to the SDF
--   equivalent, which uses double-quotes.
convertQuotes :: String -> String
convertQuotes ('\'':str) = '\"':(convertQuotes' str)
   where convertQuotes' ['\''] = "\""
         convertQuotes' ('\\':'\'':str) = "\\\'" ++ convertQuotes' str
         convertQuotes' (x:str) = x:(convertQuotes' str)
         convertQuotes' []     = error $ "Single quote missing in end of string" ++ str
convertQuotes str = error $ "Single quote missing in beginning of string" ++ str

