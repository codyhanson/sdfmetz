module Language.Sdf.ShowInstances where

import Language.Sdf.Syntax

{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Syntax.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Show Grammar where
    showsPrec d (Sdf_aliases aa) = showParen (d >= 10)
	      (showString "Sdf_aliases" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_restrictions aa) = showParen (d >= 10)
	      (showString "Sdf_restrictions" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_sorts_ aa) = showParen (d >= 10)
	      (showString "Sdf_sorts_" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_priorities aa) = showParen (d >= 10)
	      (showString "Sdf_priorities" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_imp_section aa) = showParen (d >= 10)
	      (showString "Sdf_imp_section" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_lexical_syntax aa) = showParen (d >= 10)
	      (showString "Sdf_lexical_syntax" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_context_free_syntax aa) = showParen (d >= 10)
	      (showString "Sdf_context_free_syntax" . showChar ' ' .
	       showsPrec 10 aa)
    showsPrec d (Sdf_variables aa) = showParen (d >= 10)
	      (showString "Sdf_variables" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_lexical_variables aa) = showParen (d >= 10)
	      (showString "Sdf_lexical_variables" . showChar ' ' .
	       showsPrec 10 aa)
    showsPrec d (Sdf_empty_grammar) = showString "Sdf_empty_grammar"
    showsPrec d (Sdf_conc_grammars aa ab) = showParen (d >= 10)
	      (showString "Sdf_conc_grammars" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_syntax aa) = showParen (d >= 10)
	      (showString "Sdf_syntax" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_lexical_priorities aa) = showParen (d >= 10)
	      (showString "Sdf_lexical_priorities" . showChar ' ' .
	       showsPrec 10 aa)
    showsPrec d (Sdf_context_free_priorities aa) = showParen (d >= 10)
	      (showString "Sdf_context_free_priorities" . showChar ' ' .
	       showsPrec 10 aa)
    showsPrec d (Sdf_lexical_restrictions aa) = showParen (d >= 10)
	      (showString "Sdf_lexical_restrictions" . showChar ' ' .
	       showsPrec 10 aa)
    showsPrec d (Sdf_context_free_restrictions aa) =
	      showParen (d >= 10)
	      (showString "Sdf_context_free_restrictions" . showChar ' ' .
	       showsPrec 10 aa)

instance Show Alias where
    showsPrec d (Sdf_alias aa ab) = showParen (d >= 10)
	      (showString "Sdf_alias" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Aliases where
    showsPrec d (Sdf_list aa) = showParen (d >= 10)
	      (showString "Sdf_list" . showChar ' ' . showsPrec 10 aa)

instance Show Lookahead where
    showsPrec d (Sdf_char_class aa) = showParen (d >= 10)
	      (showString "Sdf_char_class" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_seq aa ab) = showParen (d >= 10)
	      (showString "Sdf_seq" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Lookaheads where
    showsPrec d (Sdf_single aa) = showParen (d >= 10)
	      (showString "Sdf_single" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_alt aa ab) = showParen (d >= 10)
	      (showString "Sdf_alt" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_list1 aa) = showParen (d >= 10)
	      (showString "Sdf_list1" . showChar ' ' . showsPrec 10 aa)

instance Show Restriction where
    showsPrec d (Sdf_follow aa ab) = showParen (d >= 10)
	      (showString "Sdf_follow" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Restrictions where
    showsPrec d (Sdf_list2 aa) = showParen (d >= 10)
	      (showString "Sdf_list2" . showChar ' ' . showsPrec 10 aa)

instance Show Attribute where
    showsPrec d (Sdf_reject) = showString "Sdf_reject"
    showsPrec d (Sdf_prefer) = showString "Sdf_prefer"
    showsPrec d (Sdf_avoid) = showString "Sdf_avoid"
    showsPrec d (Sdf_cons1 aa) = showParen (d >= 10)
	      (showString "Sdf_cons1" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_constructor) = showString "Sdf_constructor"
    showsPrec d (Sdf_memo) = showString "Sdf_memo"
    showsPrec d (Sdf_traverse) = showString "Sdf_traverse"
    showsPrec d (Sdf_bracket) = showString "Sdf_bracket"
    showsPrec d (Sdf_atr aa) = showParen (d >= 10)
	      (showString "Sdf_atr" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_id aa) = showParen (d >= 10)
	      (showString "Sdf_id" . showChar ' ' . showsPrec 10 aa)

instance Show OptExp where
    showsPrec d (Sdf_present aa) = showParen (d >= 10)
	      (showString "Sdf_present" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_absent) = showString "Sdf_absent"

instance Show RealCon where
    showsPrec d (Sdf_real_con aa ab ac) = showParen (d >= 10)
	      (showString "Sdf_real_con" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show AFun where
    showsPrec d (Sdf_Literal aa) = showParen (d >= 10)
	      (showString "Sdf_Literal" . showChar ' ' . showsPrec 10 aa)

instance Show A_Term where
    showsPrec d (Sdf_fun aa) = showParen (d >= 10)
	      (showString "Sdf_fun" . showChar ' ' . showsPrec 10 aa)

instance Show Symbol where
    showsPrec d (Sdf_label aa ab) = showParen (d >= 10)
	      (showString "Sdf_label" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_lit aa) = showParen (d >= 10)
	      (showString "Sdf_lit" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_sort aa) = showParen (d >= 10)
	      (showString "Sdf_sort" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_char_class1 aa) = showParen (d >= 10)
	      (showString "Sdf_char_class1" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_empty1) = showString "Sdf_empty1"
    showsPrec d (Sdf_seq1 aa ab) = showParen (d >= 10)
	      (showString "Sdf_seq1" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_opt aa) = showParen (d >= 10)
	      (showString "Sdf_opt" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_iter aa) = showParen (d >= 10)
	      (showString "Sdf_iter" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_iter_star aa) = showParen (d >= 10)
	      (showString "Sdf_iter_star" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_iter_sep aa ab) = showParen (d >= 10)
	      (showString "Sdf_iter_sep" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_iter_star_sep aa ab) = showParen (d >= 10)
	      (showString "Sdf_iter_star_sep" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_iter_n aa ab) = showParen (d >= 10)
	      (showString "Sdf_iter_n" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_iter_sep_n aa ab ac) = showParen (d >= 10)
	      (showString "Sdf_iter_sep_n" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (Sdf_set aa) = showParen (d >= 10)
	      (showString "Sdf_set" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_pair aa ab) = showParen (d >= 10)
	      (showString "Sdf_pair" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_func aa ab) = showParen (d >= 10)
	      (showString "Sdf_func" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_alt1 aa ab) = showParen (d >= 10)
	      (showString "Sdf_alt1" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_perm aa) = showParen (d >= 10)
	      (showString "Sdf_perm" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_cf aa) = showParen (d >= 10)
	      (showString "Sdf_cf" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_lex aa) = showParen (d >= 10)
	      (showString "Sdf_lex" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_varsym aa) = showParen (d >= 10)
	      (showString "Sdf_varsym" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_layout) = showString "Sdf_layout"
    showsPrec d (Sdf_start) = showString "Sdf_start"
    showsPrec d (Sdf_file_start) = showString "Sdf_file_start"

instance Show Literal where
    showsPrec d (Sdf_quoted aa) = showParen (d >= 10)
	      (showString "Sdf_quoted" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_uqlit aa) = showParen (d >= 10)
	      (showString "Sdf_uqlit" . showChar ' ' . showsPrec 10 aa)

instance Show Production where
    showsPrec d (Sdf_prod_fun aa ab ac ad) = showParen (d >= 10)
	      (showString "Sdf_prod_fun" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac
	       . showChar ' ' . showsPrec 10 ad)
    showsPrec d (Sdf_prod aa ab ac) = showParen (d >= 10)
	      (showString "Sdf_prod" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show Character where
    showsPrec d (Sdf_numeric aa) = showParen (d >= 10)
	      (showString "Sdf_numeric" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_short aa) = showParen (d >= 10)
	      (showString "Sdf_short" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_top) = showString "Sdf_top"
    showsPrec d (Sdf_eof) = showString "Sdf_eof"
    showsPrec d (Sdf_bot) = showString "Sdf_bot"
    showsPrec d (Sdf_label_start) = showString "Sdf_label_start"

instance Show CharRange where
    showsPrec d (Sdf_Character aa) = showParen (d >= 10)
	      (showString "Sdf_Character" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_range aa ab) = showParen (d >= 10)
	      (showString "Sdf_range" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show CharRanges where
    showsPrec d (Sdf_CharRange aa) = showParen (d >= 10)
	      (showString "Sdf_CharRange" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_conc aa ab) = showParen (d >= 10)
	      (showString "Sdf_conc" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show OptCharRanges where
    showsPrec d (Sdf_absent1) = showString "Sdf_absent1"
    showsPrec d (Sdf_present1 aa) = showParen (d >= 10)
	      (showString "Sdf_present1" . showChar ' ' . showsPrec 10 aa)

instance Show CharClass where
    showsPrec d (Sdf_simple_charclass aa) = showParen (d >= 10)
	      (showString "Sdf_simple_charclass" . showChar ' ' .
	       showsPrec 10 aa)
    showsPrec d (Sdf_comp aa) = showParen (d >= 10)
	      (showString "Sdf_comp" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_diff aa ab) = showParen (d >= 10)
	      (showString "Sdf_diff" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_isect aa ab) = showParen (d >= 10)
	      (showString "Sdf_isect" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_union aa ab) = showParen (d >= 10)
	      (showString "Sdf_union" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Associativity where
    showsPrec d (Sdf_left) = showString "Sdf_left"
    showsPrec d (Sdf_right) = showString "Sdf_right"
    showsPrec d (Sdf_non_assoc) = showString "Sdf_non_assoc"
    showsPrec d (Sdf_assoc) = showString "Sdf_assoc"

instance Show Group where
    showsPrec d (Sdf_simple_group aa) = showParen (d >= 10)
	      (showString "Sdf_simple_group" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_prods_group aa) = showParen (d >= 10)
	      (showString "Sdf_prods_group" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_assoc_group aa ab) = showParen (d >= 10)
	      (showString "Sdf_assoc_group" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Priority where
    showsPrec d (Sdf_chain aa) = showParen (d >= 10)
	      (showString "Sdf_chain" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_assoc1 aa ab ac) = showParen (d >= 10)
	      (showString "Sdf_assoc1" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show Priorities where
    showsPrec d (Sdf_comma aa) = showParen (d >= 10)
	      (showString "Sdf_comma" . showChar ' ' . showsPrec 10 aa)

instance Show IntCon where
    showsPrec d (Sdf_natural aa) = showParen (d >= 10)
	      (showString "Sdf_natural" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_positive aa) = showParen (d >= 10)
	      (showString "Sdf_positive" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_negative aa) = showParen (d >= 10)
	      (showString "Sdf_negative" . showChar ' ' . showsPrec 10 aa)

instance Show Renamings where
    showsPrec d (Sdf_renamings aa) = showParen (d >= 10)
	      (showString "Sdf_renamings" . showChar ' ' . showsPrec 10 aa)

instance Show Renaming where
    showsPrec d (Sdf_symbol aa ab) = showParen (d >= 10)
	      (showString "Sdf_symbol" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Sdf_production aa ab) = showParen (d >= 10)
	      (showString "Sdf_production" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Definition where
    showsPrec d (Sdf_list4 aa) = showParen (d >= 10)
	      (showString "Sdf_list4" . showChar ' ' . showsPrec 10 aa)

instance Show Module where
    showsPrec d (Sdf_module_ aa ab ac) = showParen (d >= 10)
	      (showString "Sdf_module_" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show Section where
    showsPrec d (Sdf_exports_ aa) = showParen (d >= 10)
	      (showString "Sdf_exports_" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_hiddens aa) = showParen (d >= 10)
	      (showString "Sdf_hiddens" . showChar ' ' . showsPrec 10 aa)

instance Show Sections where
    showsPrec d (Sdf_list5 aa) = showParen (d >= 10)
	      (showString "Sdf_list5" . showChar ' ' . showsPrec 10 aa)

instance Show ModuleName where
    showsPrec d (Sdf_unparameterized aa) = showParen (d >= 10)
	      (showString "Sdf_unparameterized" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_parameterized aa ab) = showParen (d >= 10)
	      (showString "Sdf_parameterized" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show ImpSection where
    showsPrec d (Sdf_imports_ aa) = showParen (d >= 10)
	      (showString "Sdf_imports_" . showChar ' ' . showsPrec 10 aa)

instance Show Imports where
    showsPrec d (Sdf_list6 aa) = showParen (d >= 10)
	      (showString "Sdf_list6" . showChar ' ' . showsPrec 10 aa)

instance Show Import where
    showsPrec d (Sdf_module1 aa) = showParen (d >= 10)
	      (showString "Sdf_module1" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_renamed_module aa ab) = showParen (d >= 10)
	      (showString "Sdf_renamed_module" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Symbols where
    showsPrec d (Sdf_list7 aa) = showParen (d >= 10)
	      (showString "Sdf_list7" . showChar ' ' . showsPrec 10 aa)

instance Show Attributes where
    showsPrec d (Sdf_attrs aa) = showParen (d >= 10)
	      (showString "Sdf_attrs" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Sdf_no_attrs) = showString "Sdf_no_attrs"

instance Show Productions where
    showsPrec d (Sdf_list8 aa) = showParen (d >= 10)
	      (showString "Sdf_list8" . showChar ' ' . showsPrec 10 aa)

instance Show SDF where
    showsPrec d (Sdf_definition aa) = showParen (d >= 10)
	      (showString "Sdf_definition" . showChar ' ' . showsPrec 10 aa)

--  Imported from other files :-
