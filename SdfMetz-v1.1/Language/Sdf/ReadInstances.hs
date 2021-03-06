module Language.Sdf.ReadInstances where

import Language.Sdf.Syntax

{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Syntax.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Read Grammar where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_aliases aa) , rest) | ("Sdf_aliases" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_restrictions aa) , rest) |
		("Sdf_restrictions" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_sorts_ aa) , rest) | ("Sdf_sorts_" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_priorities aa) , rest) | ("Sdf_priorities" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_imp_section aa) , rest) |
		("Sdf_imp_section" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_lexical_syntax aa) , rest) |
		("Sdf_lexical_syntax" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_context_free_syntax aa) , rest) |
		("Sdf_context_free_syntax" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_variables aa) , rest) | ("Sdf_variables" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_lexical_variables aa) , rest) |
		("Sdf_lexical_variables" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_empty_grammar) , rest) |
		("Sdf_empty_grammar" , rest) <- lex inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_conc_grammars aa ab) , rest) |
		("Sdf_conc_grammars" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_syntax aa) , rest) | ("Sdf_syntax" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_lexical_priorities aa) , rest) |
		("Sdf_lexical_priorities" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_context_free_priorities aa) , rest) |
		("Sdf_context_free_priorities" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_lexical_restrictions aa) , rest) |
		("Sdf_lexical_restrictions" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_context_free_restrictions aa) , rest) |
		("Sdf_context_free_restrictions" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input

instance Read Alias where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_alias aa ab) , rest) | ("Sdf_alias" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input

instance Read Aliases where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list aa) , rest) | ("Sdf_list" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Lookahead where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_char_class aa) , rest) | ("Sdf_char_class" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_seq aa ab) , rest) | ("Sdf_seq" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input

instance Read Lookaheads where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_single aa) , rest) | ("Sdf_single" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_alt aa ab) , rest) | ("Sdf_alt" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list1 aa) , rest) | ("Sdf_list1" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Restriction where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_follow aa ab) , rest) | ("Sdf_follow" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input

instance Read Restrictions where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list2 aa) , rest) | ("Sdf_list2" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Attribute where
    readsPrec d input =
	      (\ inp ->
	       [((Sdf_reject) , rest) | ("Sdf_reject" , rest) <- lex inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_prefer) , rest) | ("Sdf_prefer" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_avoid) , rest) | ("Sdf_avoid" , rest) <- lex inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_cons1 aa) , rest) | ("Sdf_cons1" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_constructor) , rest) |
		("Sdf_constructor" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_memo) , rest) | ("Sdf_memo" , rest) <- lex inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_traverse) , rest) | ("Sdf_traverse" , rest) <- lex inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_bracket) , rest) | ("Sdf_bracket" , rest) <- lex inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_atr aa) , rest) | ("Sdf_atr" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_id aa) , rest) | ("Sdf_id" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read OptExp where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_present aa) , rest) | ("Sdf_present" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_absent) , rest) | ("Sdf_absent" , rest) <- lex inp])
	      input

instance Read RealCon where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_real_con aa ab ac) , rest) |
		("Sdf_real_con" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp , (ab , inp) <- readsPrec 10 inp ,
		(ac , rest) <- readsPrec 10 inp])
	      input

instance Read AFun where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_Literal aa) , rest) | ("Sdf_Literal" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read A_Term where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_fun aa) , rest) | ("Sdf_fun" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Symbol where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_label aa ab) , rest) | ("Sdf_label" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_lit aa) , rest) | ("Sdf_lit" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_sort aa) , rest) | ("Sdf_sort" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_char_class1 aa) , rest) |
		("Sdf_char_class1" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_empty1) , rest) | ("Sdf_empty1" , rest) <- lex inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_seq1 aa ab) , rest) | ("Sdf_seq1" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_opt aa) , rest) | ("Sdf_opt" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_iter aa) , rest) | ("Sdf_iter" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_iter_star aa) , rest) | ("Sdf_iter_star" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_iter_sep aa ab) , rest) | ("Sdf_iter_sep" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_iter_star_sep aa ab) , rest) |
		("Sdf_iter_star_sep" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_iter_n aa ab) , rest) | ("Sdf_iter_n" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_iter_sep_n aa ab ac) , rest) |
		("Sdf_iter_sep_n" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp , (ab , inp) <- readsPrec 10 inp ,
		(ac , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_set aa) , rest) | ("Sdf_set" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_pair aa ab) , rest) | ("Sdf_pair" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_func aa ab) , rest) | ("Sdf_func" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_alt1 aa ab) , rest) | ("Sdf_alt1" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_perm aa) , rest) | ("Sdf_perm" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_cf aa) , rest) | ("Sdf_cf" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_lex aa) , rest) | ("Sdf_lex" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_varsym aa) , rest) | ("Sdf_varsym" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_layout) , rest) | ("Sdf_layout" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_start) , rest) | ("Sdf_start" , rest) <- lex inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_file_start) , rest) | ("Sdf_file_start" , rest) <- lex inp])
	      input

instance Read Literal where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_quoted aa) , rest) | ("Sdf_quoted" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_uqlit aa) , rest) | ("Sdf_uqlit" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Production where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_prod_fun aa ab ac ad) , rest) |
		("Sdf_prod_fun" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp , (ab , inp) <- readsPrec 10 inp ,
		(ac , inp) <- readsPrec 10 inp , (ad , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_prod aa ab ac) , rest) | ("Sdf_prod" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , inp) <- readsPrec 10 inp ,
		(ac , rest) <- readsPrec 10 inp])
	      input

instance Read Character where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_numeric aa) , rest) | ("Sdf_numeric" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_short aa) , rest) | ("Sdf_short" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp -> [((Sdf_top) , rest) | ("Sdf_top" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_eof) , rest) | ("Sdf_eof" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_bot) , rest) | ("Sdf_bot" , rest) <- lex inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_label_start) , rest) |
		("Sdf_label_start" , rest) <- lex inp])
	      input

instance Read CharRange where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_Character aa) , rest) | ("Sdf_Character" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_range aa ab) , rest) | ("Sdf_range" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input

instance Read CharRanges where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_CharRange aa) , rest) | ("Sdf_CharRange" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_conc aa ab) , rest) | ("Sdf_conc" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input

instance Read OptCharRanges where
    readsPrec d input =
	      (\ inp ->
	       [((Sdf_absent1) , rest) | ("Sdf_absent1" , rest) <- lex inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_present1 aa) , rest) | ("Sdf_present1" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read CharClass where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_simple_charclass aa) , rest) |
		("Sdf_simple_charclass" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_comp aa) , rest) | ("Sdf_comp" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_diff aa ab) , rest) | ("Sdf_diff" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_isect aa ab) , rest) | ("Sdf_isect" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_union aa ab) , rest) | ("Sdf_union" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input

instance Read Associativity where
    readsPrec d input =
	      (\ inp -> [((Sdf_left) , rest) | ("Sdf_left" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_right) , rest) | ("Sdf_right" , rest) <- lex inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_non_assoc) , rest) | ("Sdf_non_assoc" , rest) <- lex inp])
	      input
	      ++
	      (\ inp -> [((Sdf_assoc) , rest) | ("Sdf_assoc" , rest) <- lex inp])
	      input

instance Read Group where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_simple_group aa) , rest) |
		("Sdf_simple_group" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_prods_group aa) , rest) |
		("Sdf_prods_group" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_assoc_group aa ab) , rest) |
		("Sdf_assoc_group" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input

instance Read Priority where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_chain aa) , rest) | ("Sdf_chain" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_assoc1 aa ab ac) , rest) | ("Sdf_assoc1" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , inp) <- readsPrec 10 inp ,
		(ac , rest) <- readsPrec 10 inp])
	      input

instance Read Priorities where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_comma aa) , rest) | ("Sdf_comma" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read IntCon where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_natural aa) , rest) | ("Sdf_natural" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_positive aa) , rest) | ("Sdf_positive" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_negative aa) , rest) | ("Sdf_negative" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Renamings where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_renamings aa) , rest) | ("Sdf_renamings" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Renaming where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_symbol aa ab) , rest) | ("Sdf_symbol" , inp) <- lex inp ,
		(aa , inp) <- readsPrec 10 inp , (ab , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_production aa ab) , rest) |
		("Sdf_production" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input

instance Read Definition where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list4 aa) , rest) | ("Sdf_list4" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Module where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_module_ aa ab ac) , rest) | ("Sdf_module_" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp , (ab , inp) <- readsPrec 10 inp ,
		(ac , rest) <- readsPrec 10 inp])
	      input

instance Read Section where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_exports_ aa) , rest) | ("Sdf_exports_" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_hiddens aa) , rest) | ("Sdf_hiddens" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Sections where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list5 aa) , rest) | ("Sdf_list5" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read ModuleName where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_unparameterized aa) , rest) |
		("Sdf_unparameterized" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_parameterized aa ab) , rest) |
		("Sdf_parameterized" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input

instance Read ImpSection where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_imports_ aa) , rest) | ("Sdf_imports_" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Imports where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list6 aa) , rest) | ("Sdf_list6" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Import where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_module1 aa) , rest) | ("Sdf_module1" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_renamed_module aa ab) , rest) |
		("Sdf_renamed_module" , inp) <- lex inp
		, (aa , inp) <- readsPrec 10 inp ,
		(ab , rest) <- readsPrec 10 inp])
	      input

instance Read Symbols where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list7 aa) , rest) | ("Sdf_list7" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read Attributes where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_attrs aa) , rest) | ("Sdf_attrs" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input
	      ++
	      (\ inp ->
	       [((Sdf_no_attrs) , rest) | ("Sdf_no_attrs" , rest) <- lex inp])
	      input

instance Read Productions where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_list8 aa) , rest) | ("Sdf_list8" , inp) <- lex inp ,
		(aa , rest) <- readsPrec 10 inp])
	      input

instance Read SDF where
    readsPrec d input =
	      readParen (d > 9)
	      (\ inp ->
	       [((Sdf_definition aa) , rest) | ("Sdf_definition" , inp) <- lex inp
		, (aa , rest) <- readsPrec 10 inp])
	      input

--  Imported from other files :-
