module Language.Antlr.EqInstances where

import Language.Antlr.Syntax

{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Syntax.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Eq GrammarHeader where
    (GrammarHeader aa ab) == (GrammarHeader aa' ab') = aa == aa' &&
			  ab == ab'
    _ == _ = False

instance Eq Grammar where
    (Grammar aa ab ac) == (Grammar aa' ab' ac') = aa == aa' &&
		       ab == ab' && ac == ac'
    _ == _ = False

instance Eq ClassDef where
    (ClassDef aa ab ac ad) == (ClassDef aa' ab' ac' ad') = aa == aa' &&
			   ab == ab' && ac == ac' && ad == ad'
    _ == _ = False

instance Eq OptionAssignmentSemi where
    (OptionAssignmentSemi aa ab) == (OptionAssignmentSemi aa' ab') =
				 aa == aa' && ab == ab'
    _ == _ = False

instance Eq OptionAssignment where
    (OptionAssignment aa ab) == (OptionAssignment aa' ab') = aa == aa'
			     && ab == ab'
    _ == _ = False

instance Eq OptionsSpec where
    (OptionsSpec aa) == (OptionsSpec aa') = aa == aa'
    _ == _ = False

instance Eq SingleOptionSpec where
    (LexOption aa) == (LexOption aa') = aa == aa'
    (Option aa ab) == (Option aa' ab') = aa == aa' && ab == ab'
    _ == _ = False

instance Eq LexerOptionsSpec where
    (LexerOptionsSpec aa) == (LexerOptionsSpec aa') = aa == aa'
    _ == _ = False

instance Eq OptionValue where
    (IdValue aa) == (IdValue aa') = aa == aa'
    (StringValue aa) == (StringValue aa') = aa == aa'
    (CharValue aa) == (CharValue aa') = aa == aa'
    (IntValue aa) == (IntValue aa') = aa == aa'
    _ == _ = False

instance Eq CharSet where
    (CharSet aa) == (CharSet aa') = aa == aa'
    _ == _ = False

instance Eq SetBlockElement where
    (SetBlockElement aa ab) == (SetBlockElement aa' ab') = aa == aa' &&
			    ab == ab'
    _ == _ = False

instance Eq SingleTokenSpec where
    (TokenRefSpec aa ab ac) == (TokenRefSpec aa' ab' ac') = aa == aa'
			    && ab == ab' && ac == ac'
    (StringLitTokenSpec aa ab) == (StringLitTokenSpec aa' ab') =
			       aa == aa' && ab == ab'
    _ == _ = False

instance Eq TokensSpec where
    (TokensSpec aa) == (TokensSpec aa') = aa == aa'
    _ == _ = False

instance Eq TokensSpecOptions where
    (TokensSpecOptions aa) == (TokensSpecOptions aa') = aa == aa'
    _ == _ = False

instance Eq SuperClass where
    (SuperClass aa) == (SuperClass aa') = aa == aa'
    _ == _ = False

instance Eq ClassHeader where
    (LexClassHeader aa) == (LexClassHeader aa') = aa == aa'
    (ClassHeader aa ab ac) == (ClassHeader aa' ab' ac') = aa == aa' &&
			   ab == ab' && ac == ac'
    _ == _ = False

instance Eq Spec where
    (Spec aa ab ac ad) == (Spec aa' ab' ac' ad') = aa == aa' &&
		       ab == ab' && ac == ac' && ad == ad'
    _ == _ = False

instance Eq Rule where
    (Rule aa ab ac ad ae af ag ah ai aj ak) ==
					    (Rule aa' ab' ac' ad' ae' af' ag' ah' ai' aj' ak') =
					    aa == aa' && ab == ab' && ac == ac' && ad == ad' &&
					    ae == ae' && af == af' && ag == ag' && ah == ah' &&
					    ai == ai' && aj == aj' && ak == ak'
    _ == _ = False

instance Eq ThrowsSpec where
    (ThrowsSpec aa) == (ThrowsSpec aa') = aa == aa'
    _ == _ = False

instance Eq Block where
    (Block aa) == (Block aa') = aa == aa'
    _ == _ = False

instance Eq Alternative where
    (Alternative aa ab ac) == (Alternative aa' ab' ac') = aa == aa' &&
			   ab == ab' && ac == ac'
    _ == _ = False

instance Eq ExceptionSpec where
    (ExceptionSpec aa ab) == (ExceptionSpec aa' ab') = aa == aa' &&
			  ab == ab'
    _ == _ = False

instance Eq ExceptionSpecNoLabel where
    (ExceptionSpecNoLabel aa) == (ExceptionSpecNoLabel aa') = aa == aa'
    _ == _ = False

instance Eq ExceptionHandler where
    (ExceptionHandler aa ab) == (ExceptionHandler aa' ab') = aa == aa'
			     && ab == ab'
    _ == _ = False

instance Eq Element where
    (Element aa ab) == (Element aa' ab') = aa == aa' && ab == ab'
    _ == _ = False

instance Eq ElementOptionSpec where
    (ElementOptionSpec aa) == (ElementOptionSpec aa') = aa == aa'
    _ == _ = False

instance Eq AssignElementBody where
    (RuleRefElt aa ab ac) == (RuleRefElt aa' ab' ac') = aa == aa' &&
			  ab == ab' && ac == ac'
    (TokenRefElt aa ab) == (TokenRefElt aa' ab') = aa == aa' &&
			ab == ab'
    _ == _ = False

instance Eq NegatedElement where
    (NegatedNotTerminal aa) == (NegatedNotTerminal aa') = aa == aa'
    (NegatedEbnf aa) == (NegatedEbnf aa') = aa == aa'
    _ == _ = False

instance Eq NoAssignElementBody where
    (RuleRefEltNoAssign aa ab ac) == (RuleRefEltNoAssign aa' ab' ac') =
				  aa == aa' && ab == ab' && ac == ac'
    (RangeElt aa) == (RangeElt aa') = aa == aa'
    (TerminalElt aa) == (TerminalElt aa') = aa == aa'
    (NegatedElt aa) == (NegatedElt aa') = aa == aa'
    (EbnfElt aa) == (EbnfElt aa') = aa == aa'
    _ == _ = False

instance Eq ElementNoOptionSpec where
    (AssignElement aa ab ac) == (AssignElement aa' ab' ac') = aa == aa'
			     && ab == ab' && ac == ac'
    (NoAssignElement aa ab) == (NoAssignElement aa' ab') = aa == aa' &&
			    ab == ab'
    (ActionElement aa) == (ActionElement aa') = aa == aa'
    (SemPredElement aa) == (SemPredElement aa') = aa == aa'
    (TreeElement aa) == (TreeElement aa') = aa == aa'
    _ == _ = False

instance Eq Tree where
    (Tree aa ab) == (Tree aa' ab') = aa == aa' && ab == ab'
    _ == _ = False

instance Eq RootNode where
    (RootNode aa ab) == (RootNode aa' ab') = aa == aa' && ab == ab'
    _ == _ = False

instance Eq OptionsSpecOrAction where
    (OptionsSpecAndMaybeAction aa ab) ==
				      (OptionsSpecAndMaybeAction aa' ab') = aa == aa' && ab == ab'
    (Action aa) == (Action aa') = aa == aa'
    _ == _ = False

instance Eq RegExpOperator where
    (Question aa) == (Question aa') = aa == aa'
    (Star aa) == (Star aa') = aa == aa'
    (Plus aa) == (Plus aa') = aa == aa'
    _ == _ = False

instance Eq EbnfBody where
    (EbnfBodyRegExp aa ab) == (EbnfBodyRegExp aa' ab') = aa == aa' &&
			   ab == ab'
    (EbnfBodyImplies aa) == (EbnfBodyImplies aa') = aa == aa'
    _ == _ = False

instance Eq Ebnf where
    (Ebnf aa ab ac) == (Ebnf aa' ab' ac') = aa == aa' && ab == ab' &&
		    ac == ac'
    _ == _ = False

instance Eq Ast_type_spec where
    (CaretAstType aa) == (CaretAstType aa') = aa == aa'
    (BangAstType aa) == (BangAstType aa') = aa == aa'
    NoAstType == NoAstType = True
    _ == _ = False

instance Eq TokenRefOrStringLit where
    (TokenRef aa) == (TokenRef aa') = aa == aa'
    (StringLit aa) == (StringLit aa') = aa == aa'
    _ == _ = False

instance Eq Range where
    (CharRange aa ab ac) == (CharRange aa' ab' ac') = aa == aa' &&
			 ab == ab' && ac == ac'
    (StringTokenRange aa ab ac) == (StringTokenRange aa' ab' ac') =
				aa == aa' && ab == ab' && ac == ac'
    _ == _ = False

instance Eq Terminal where
    (CharLiteralT aa ab) == (CharLiteralT aa' ab') = aa == aa' &&
			 ab == ab'
    (TokenRefT aa ab ac) == (TokenRefT aa' ab' ac') = aa == aa' &&
			 ab == ab' && ac == ac'
    (StringLiteralT aa ab) == (StringLiteralT aa' ab') = aa == aa' &&
			   ab == ab'
    (WildCardT aa) == (WildCardT aa') = aa == aa'
    _ == _ = False

instance Eq NotTerminal where
    (CharLiteralNT aa ab) == (CharLiteralNT aa' ab') = aa == aa' &&
			  ab == ab'
    (TokenRefNT aa ab) == (TokenRefNT aa' ab') = aa == aa' && ab == ab'
    _ == _ = False

instance Eq QualifiedID where
    (QualifiedID aa) == (QualifiedID aa') = aa == aa'
    _ == _ = False

instance Eq Id where
    (TokenRefId aa) == (TokenRefId aa') = aa == aa'
    (RuleRefId aa) == (RuleRefId aa') = aa == aa'
    _ == _ = False

--  Imported from other files :-
