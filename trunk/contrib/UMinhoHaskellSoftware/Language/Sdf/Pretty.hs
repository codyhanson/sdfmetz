-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Sdf.Pretty
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty printing Sdf definitions.
--
-----------------------------------------------------------------------------

module Language.Sdf.Pretty where

import GPP
import Language.Sdf.Syntax
import Language.Sdf.SyntaxPP

import StrategyLib
import Language.Sdf.TermInstances

import Text.PrettyPrint.HughesPJ

-----------------------------------------------------------------------------
-- * Pretty-printing specific grammar elements

-- | Pretty-print a complete SDF definition to a String.
renderSDF :: SDF -> String
renderSDF = renderSdf

-----------------------------------------------------------------------------
-- * Pretty-printing arbitrary grammar elements

-- | Pretty-print any Sdf fragment.
renderSdf :: (Term a) => a -> String
renderSdf a = renderFix uppSyntaxCustom a

-- | Pretty-print any Sdf fragment, using a given pretty-print mode.
renderSdfMode :: (Term a) => Mode -> a -> String
renderSdfMode mode a = renderFixMode mode uppSyntaxCustom a

-- | Customized pretty-printer.
uppSyntaxCustom :: UPP
uppSyntaxCustom gpp
  = uppSyntax gpp
      `adhocQ` (ppSymbol gpp)
      `adhocQ` (ppProduction gpp)
      `adhocQ` (ppModule gpp)
      `adhocQ` (ppGrammar gpp)
      `adhocQ` (ppSDF gpp)
    where
      ppSymbol :: GPP -> Symbol -> Doc
      ppSymbol gpp (Sdf_empty1)       = gpp "(" <> gpp ")"
      ppSymbol gpp (Sdf_opt _0)       = gpp _0 <> gpp "?"
      ppSymbol gpp (Sdf_iter _0)      = gpp _0 <> gpp "+"
      ppSymbol gpp (Sdf_iter_star _0) = gpp _0 <> gpp "*"
      ppSymbol gpp (Sdf_iter_sep _1 _2)
          = gpp "{" <> gpp _1 <+> gpp _2 <> gpp "}" <> gpp "+"
      ppSymbol gpp (Sdf_iter_star_sep _1 _2)
          = gpp "{" <> gpp _1 <+> gpp _2 <> gpp "}" <> gpp "*"
      ppSymbol gpp (Sdf_iter_n _0 _1) = gpp _0 <+> gpp _1 <> gpp "+"
      ppSymbol gpp (Sdf_iter_sep_n _1 _2 _4)
          = gpp "{" <> gpp _1 <+> gpp _2 <> gpp "}"<> gpp _4 <> gpp "+"
      ppSymbol gpp (Sdf_cf _1) = gpp "<" <> gpp _1 <> gpp "-CF" <> gpp ">"
      ppSymbol gpp (Sdf_lex _1) = fcat [gpp "<", gpp _1, gpp "-LEX", gpp ">"]
      ppSymbol gpp s            = pp gpp s     
      
      ppModule :: GPP -> Module -> Doc
      ppModule gpp (Sdf_module_ _1 _2 _3)
	    = vcat [ hsep [gpp "module", gpp _1] , gppList gpp _2, gpp _3]
      
      ppSDF :: GPP -> SDF -> Doc 
      ppSDF gpp (Sdf_definition _1) = vcat [gpp "definition", gpp _1]
      
      ppGrammar :: GPP -> Grammar -> Doc
      ppGrammar gpp (Sdf_aliases _1) = fsep [gpp "aliases", gpp _1]
      ppGrammar gpp (Sdf_restrictions _1) = fsep [gpp "restrictions", gpp _1]
      ppGrammar gpp (Sdf_sorts_ (Sdf_list7 _1)) 
        = vcat [gpp "sorts", nest 2 (fsep $ map gpp _1)]
      ppGrammar gpp (Sdf_priorities _1) = fsep [gpp "priorities", gpp _1]
      ppGrammar gpp (Sdf_imp_section _0) = fsep [gpp _0]
      ppGrammar gpp (Sdf_lexical_syntax _2)
	    = vcat [ hsep [gpp "lexical", gpp "syntax"], nest 2 (gpp _2)]
      ppGrammar gpp (Sdf_context_free_syntax _2)
	    = vcat [ hsep [gpp "context-free", gpp "syntax"], nest 2 (gpp _2)]
      ppGrammar gpp (Sdf_variables _1) = fsep [gpp "variables", gpp _1]
      ppGrammar gpp (Sdf_lexical_variables _2)
	    = fsep [gpp "lexical", gpp "variables", gpp _2]
      ppGrammar gpp (Sdf_empty_grammar) = fsep [gpp "(/)"]
      ppGrammar gpp (Sdf_conc_grammars _0 _1) = fsep [gpp _0, gpp _1]
      ppGrammar gpp (Sdf_syntax _1) = fsep [gpp "syntax", gpp _1]
      ppGrammar gpp (Sdf_lexical_priorities _2)
	    = fsep [gpp "lexical", gpp "priorities", gpp _2]
      ppGrammar gpp (Sdf_context_free_priorities _2)
	    = fsep [gpp "context-free", gpp "priorities", gpp _2]
      ppGrammar gpp (Sdf_lexical_restrictions _2)
	    = fsep [gpp "lexical", gpp "restrictions", gpp _2]
      ppGrammar gpp (Sdf_context_free_restrictions _2)
	    = fsep [gpp "context-free", gpp "restrictions", gpp _2]
  
      ppProduction :: GPP -> Production -> Doc
      ppProduction gpp (Sdf_prod_fun _0 _2 _5 _6)
        = hang (hsep [gpp _0, gpp "(", gppListSep gpp "," _2, gpp ")"])
           2 (hsep [gpp "->", gpp _5, gpp _6])
      ppProduction gpp (Sdf_prod (Sdf_list7 _0) _2 _3)
        = hang (fsep $ map gpp _0)
           2 (hsep [gpp "->", gpp _2, gpp _3])
           



-----------------------------------------------------------------------------
