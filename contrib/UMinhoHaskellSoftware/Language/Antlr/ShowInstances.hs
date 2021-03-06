module Language.Antlr.ShowInstances where

import Language.Antlr.Syntax

{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Syntax.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Show GrammarHeader where
    showsPrec d (GrammarHeader aa ab) = showParen (d >= 10)
	      (showString "GrammarHeader" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Grammar where
    showsPrec d (Grammar aa ab ac) = showParen (d >= 10)
	      (showString "Grammar" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show ClassDef where
    showsPrec d (ClassDef aa ab ac ad) = showParen (d >= 10)
	      (showString "ClassDef" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac
	       . showChar ' ' . showsPrec 10 ad)

instance Show OptionAssignmentSemi where
    showsPrec d (OptionAssignmentSemi aa ab) = showParen (d >= 10)
	      (showString "OptionAssignmentSemi" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show OptionAssignment where
    showsPrec d (OptionAssignment aa ab) = showParen (d >= 10)
	      (showString "OptionAssignment" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show OptionsSpec where
    showsPrec d (OptionsSpec aa) = showParen (d >= 10)
	      (showString "OptionsSpec" . showChar ' ' . showsPrec 10 aa)

instance Show SingleOptionSpec where
    showsPrec d (LexOption aa) = showParen (d >= 10)
	      (showString "LexOption" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Option aa ab) = showParen (d >= 10)
	      (showString "Option" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show LexerOptionsSpec where
    showsPrec d (LexerOptionsSpec aa) = showParen (d >= 10)
	      (showString "LexerOptionsSpec" . showChar ' ' . showsPrec 10 aa)

instance Show OptionValue where
    showsPrec d (IdValue aa) = showParen (d >= 10)
	      (showString "IdValue" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (StringValue aa) = showParen (d >= 10)
	      (showString "StringValue" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (CharValue aa) = showParen (d >= 10)
	      (showString "CharValue" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (IntValue aa) = showParen (d >= 10)
	      (showString "IntValue" . showChar ' ' . showsPrec 10 aa)

instance Show CharSet where
    showsPrec d (CharSet aa) = showParen (d >= 10)
	      (showString "CharSet" . showChar ' ' . showsPrec 10 aa)

instance Show SetBlockElement where
    showsPrec d (SetBlockElement aa ab) = showParen (d >= 10)
	      (showString "SetBlockElement" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show SingleTokenSpec where
    showsPrec d (TokenRefSpec aa ab ac) = showParen (d >= 10)
	      (showString "TokenRefSpec" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (StringLitTokenSpec aa ab) = showParen (d >= 10)
	      (showString "StringLitTokenSpec" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show TokensSpec where
    showsPrec d (TokensSpec aa) = showParen (d >= 10)
	      (showString "TokensSpec" . showChar ' ' . showsPrec 10 aa)

instance Show TokensSpecOptions where
    showsPrec d (TokensSpecOptions aa) = showParen (d >= 10)
	      (showString "TokensSpecOptions" . showChar ' ' . showsPrec 10 aa)

instance Show SuperClass where
    showsPrec d (SuperClass aa) = showParen (d >= 10)
	      (showString "SuperClass" . showChar ' ' . showsPrec 10 aa)

instance Show ClassHeader where
    showsPrec d (LexClassHeader aa) = showParen (d >= 10)
	      (showString "LexClassHeader" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (ClassHeader aa ab ac) = showParen (d >= 10)
	      (showString "ClassHeader" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show Spec where
    showsPrec d (Spec aa ab ac ad) = showParen (d >= 10)
	      (showString "Spec" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac
	       . showChar ' ' . showsPrec 10 ad)

instance Show Rule where
    showsPrec d (Rule aa ab ac ad ae af ag ah ai aj ak) =
	      showParen (d >= 10)
	      (showString "Rule" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac
	       . showChar ' ' . showsPrec 10 ad . showChar ' ' . showsPrec 10 ae
	       . showChar ' ' . showsPrec 10 af . showChar ' ' . showsPrec 10 ag
	       . showChar ' ' . showsPrec 10 ah . showChar ' ' . showsPrec 10 ai
	       . showChar ' ' . showsPrec 10 aj . showChar ' ' . showsPrec 10 ak)

instance Show ThrowsSpec where
    showsPrec d (ThrowsSpec aa) = showParen (d >= 10)
	      (showString "ThrowsSpec" . showChar ' ' . showsPrec 10 aa)

instance Show Block where
    showsPrec d (Block aa) = showParen (d >= 10)
	      (showString "Block" . showChar ' ' . showsPrec 10 aa)

instance Show Alternative where
    showsPrec d (Alternative aa ab ac) = showParen (d >= 10)
	      (showString "Alternative" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show ExceptionSpec where
    showsPrec d (ExceptionSpec aa ab) = showParen (d >= 10)
	      (showString "ExceptionSpec" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show ExceptionSpecNoLabel where
    showsPrec d (ExceptionSpecNoLabel aa) = showParen (d >= 10)
	      (showString "ExceptionSpecNoLabel" . showChar ' ' .
	       showsPrec 10 aa)

instance Show ExceptionHandler where
    showsPrec d (ExceptionHandler aa ab) = showParen (d >= 10)
	      (showString "ExceptionHandler" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show Element where
    showsPrec d (Element aa ab) = showParen (d >= 10)
	      (showString "Element" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show ElementOptionSpec where
    showsPrec d (ElementOptionSpec aa) = showParen (d >= 10)
	      (showString "ElementOptionSpec" . showChar ' ' . showsPrec 10 aa)

instance Show AssignElementBody where
    showsPrec d (RuleRefElt aa ab ac) = showParen (d >= 10)
	      (showString "RuleRefElt" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (TokenRefElt aa ab) = showParen (d >= 10)
	      (showString "TokenRefElt" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show NegatedElement where
    showsPrec d (NegatedNotTerminal aa) = showParen (d >= 10)
	      (showString "NegatedNotTerminal" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (NegatedEbnf aa) = showParen (d >= 10)
	      (showString "NegatedEbnf" . showChar ' ' . showsPrec 10 aa)

instance Show NoAssignElementBody where
    showsPrec d (RuleRefEltNoAssign aa ab ac) = showParen (d >= 10)
	      (showString "RuleRefEltNoAssign" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (RangeElt aa) = showParen (d >= 10)
	      (showString "RangeElt" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (TerminalElt aa) = showParen (d >= 10)
	      (showString "TerminalElt" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (NegatedElt aa) = showParen (d >= 10)
	      (showString "NegatedElt" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (EbnfElt aa) = showParen (d >= 10)
	      (showString "EbnfElt" . showChar ' ' . showsPrec 10 aa)

instance Show ElementNoOptionSpec where
    showsPrec d (AssignElement aa ab ac) = showParen (d >= 10)
	      (showString "AssignElement" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (NoAssignElement aa ab) = showParen (d >= 10)
	      (showString "NoAssignElement" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (ActionElement aa) = showParen (d >= 10)
	      (showString "ActionElement" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (SemPredElement aa) = showParen (d >= 10)
	      (showString "SemPredElement" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (TreeElement aa) = showParen (d >= 10)
	      (showString "TreeElement" . showChar ' ' . showsPrec 10 aa)

instance Show Tree where
    showsPrec d (Tree aa ab) = showParen (d >= 10)
	      (showString "Tree" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show RootNode where
    showsPrec d (RootNode aa ab) = showParen (d >= 10)
	      (showString "RootNode" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show OptionsSpecOrAction where
    showsPrec d (OptionsSpecAndMaybeAction aa ab) = showParen (d >= 10)
	      (showString "OptionsSpecAndMaybeAction" . showChar ' ' .
	       showsPrec 10 aa . showChar ' ' . showsPrec 10 ab)
    showsPrec d (Action aa) = showParen (d >= 10)
	      (showString "Action" . showChar ' ' . showsPrec 10 aa)

instance Show RegExpOperator where
    showsPrec d (Question aa) = showParen (d >= 10)
	      (showString "Question" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Star aa) = showParen (d >= 10)
	      (showString "Star" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (Plus aa) = showParen (d >= 10)
	      (showString "Plus" . showChar ' ' . showsPrec 10 aa)

instance Show EbnfBody where
    showsPrec d (EbnfBodyRegExp aa ab) = showParen (d >= 10)
	      (showString "EbnfBodyRegExp" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (EbnfBodyImplies aa) = showParen (d >= 10)
	      (showString "EbnfBodyImplies" . showChar ' ' . showsPrec 10 aa)

instance Show Ebnf where
    showsPrec d (Ebnf aa ab ac) = showParen (d >= 10)
	      (showString "Ebnf" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show Ast_type_spec where
    showsPrec d (CaretAstType aa) = showParen (d >= 10)
	      (showString "CaretAstType" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (BangAstType aa) = showParen (d >= 10)
	      (showString "BangAstType" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (NoAstType) = showString "NoAstType"

instance Show TokenRefOrStringLit where
    showsPrec d (TokenRef aa) = showParen (d >= 10)
	      (showString "TokenRef" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (StringLit aa) = showParen (d >= 10)
	      (showString "StringLit" . showChar ' ' . showsPrec 10 aa)

instance Show Range where
    showsPrec d (CharRange aa ab ac) = showParen (d >= 10)
	      (showString "CharRange" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (StringTokenRange aa ab ac) = showParen (d >= 10)
	      (showString "StringTokenRange" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)

instance Show Terminal where
    showsPrec d (CharLiteralT aa ab) = showParen (d >= 10)
	      (showString "CharLiteralT" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (TokenRefT aa ab ac) = showParen (d >= 10)
	      (showString "TokenRefT" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab . showChar ' ' . showsPrec 10 ac)
    showsPrec d (StringLiteralT aa ab) = showParen (d >= 10)
	      (showString "StringLiteralT" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (WildCardT aa) = showParen (d >= 10)
	      (showString "WildCardT" . showChar ' ' . showsPrec 10 aa)

instance Show NotTerminal where
    showsPrec d (CharLiteralNT aa ab) = showParen (d >= 10)
	      (showString "CharLiteralNT" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)
    showsPrec d (TokenRefNT aa ab) = showParen (d >= 10)
	      (showString "TokenRefNT" . showChar ' ' . showsPrec 10 aa
	       . showChar ' ' . showsPrec 10 ab)

instance Show QualifiedID where
    showsPrec d (QualifiedID aa) = showParen (d >= 10)
	      (showString "QualifiedID" . showChar ' ' . showsPrec 10 aa)

instance Show Id where
    showsPrec d (TokenRefId aa) = showParen (d >= 10)
	      (showString "TokenRefId" . showChar ' ' . showsPrec 10 aa)
    showsPrec d (RuleRefId aa) = showParen (d >= 10)
	      (showString "RuleRefId" . showChar ' ' . showsPrec 10 aa)

--  Imported from other files :-
