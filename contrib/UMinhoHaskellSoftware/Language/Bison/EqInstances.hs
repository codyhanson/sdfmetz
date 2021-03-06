module Language.Bison.EqInstances where

import Language.Bison.Syntax

{- Generated by DrIFT (Automatic class derivations for Haskell) -}
{-# LINE 1 "Syntax.hs" #-}
{-* Generated by DrIFT : Look, but Don't Touch. *-}
instance Eq BisonSyntax where
    (BisonSyntax aa ab ac) == (BisonSyntax aa' ab' ac') = aa == aa' &&
			   ab == ab' && ac == ac'
    _ == _ = False

instance Eq Declaration where
    (GrammarDecl aa) == (GrammarDecl aa') = aa == aa'
    (PrologueDecl aa) == (PrologueDecl aa') = aa == aa'
    DebugDecl == DebugDecl = True
    (DefineDecl aa ab) == (DefineDecl aa' ab') = aa == aa' && ab == ab'
    DefinesDecl == DefinesDecl = True
    ErrorVerboseDecl == ErrorVerboseDecl = True
    (ExpectDecl aa) == (ExpectDecl aa') = aa == aa'
    (ExpectRRDecl aa ab) == (ExpectRRDecl aa' ab') = aa == aa' &&
			 ab == ab'
    (FilePrefixDecl aa) == (FilePrefixDecl aa') = aa == aa'
    GlrParserDecl == GlrParserDecl = True
    (InitialActionDecl aa) == (InitialActionDecl aa') = aa == aa'
    (LexParamDecl aa) == (LexParamDecl aa') = aa == aa'
    LocationsDecl == LocationsDecl = True
    (NamePrefixDecl aa ab) == (NamePrefixDecl aa' ab') = aa == aa' &&
			   ab == ab'
    (NoLinesDecl aa) == (NoLinesDecl aa') = aa == aa'
    NonDetParserDecl == NonDetParserDecl = True
    (OutputDecl aa) == (OutputDecl aa') = aa == aa'
    (ParseParamDecl aa) == (ParseParamDecl aa') = aa == aa'
    (PureParserDecl aa) == (PureParserDecl aa') = aa == aa'
    (SkeletonDecl aa) == (SkeletonDecl aa') = aa == aa'
    (TokenTableDecl aa) == (TokenTableDecl aa') = aa == aa'
    VerboseDecl == VerboseDecl = True
    YaccDecl == YaccDecl = True
    SemiColon == SemiColon = True
    _ == _ = False

instance Eq GrammarDeclaration where
    (LeftPriorDecl aa ab) == (LeftPriorDecl aa' ab') = aa == aa' &&
			  ab == ab'
    (RightPriorDecl aa ab) == (RightPriorDecl aa' ab') = aa == aa' &&
			   ab == ab'
    (BinaryPriorDecl aa ab) == (BinaryPriorDecl aa' ab') = aa == aa' &&
			    ab == ab'
    (NonAssocPriorDecl aa ab) == (NonAssocPriorDecl aa' ab') =
			      aa == aa' && ab == ab'
    (NTermDecl aa) == (NTermDecl aa') = aa == aa'
    (TermDecl aa) == (TermDecl aa') = aa == aa'
    (TokenDecl aa ab) == (TokenDecl aa' ab') = aa == aa' && ab == ab'
    (TypeDecl aa ab) == (TypeDecl aa' ab') = aa == aa' && ab == ab'
    (StartDecl aa) == (StartDecl aa') = aa == aa'
    (UnionDecl aa) == (UnionDecl aa') = aa == aa'
    (DestructorDecl aa ab) == (DestructorDecl aa' ab') = aa == aa' &&
			   ab == ab'
    (PrinterDecl aa ab) == (PrinterDecl aa' ab') = aa == aa' &&
			ab == ab'
    (DefaultPrecDecl aa) == (DefaultPrecDecl aa') = aa == aa'
    (NoDefaultPrecDecl aa) == (NoDefaultPrecDecl aa') = aa == aa'
    _ == _ = False

instance Eq Type where
    (Type aa) == (Type aa') = aa == aa'
    _ == _ = False

instance Eq SymbolDef where
    (TypeDef aa) == (TypeDef aa') = aa == aa'
    (IdDef aa) == (IdDef aa') = aa == aa'
    (IdIntDef aa ab) == (IdIntDef aa' ab') = aa == aa' && ab == ab'
    (IdStrLexDef aa ab) == (IdStrLexDef aa' ab') = aa == aa' &&
			ab == ab'
    (IdIntStrLexDef aa ab ac) == (IdIntStrLexDef aa' ab' ac') =
			      aa == aa' && ab == ab' && ac == ac'
    _ == _ = False

instance Eq Grammar where
    (Grammar aa) == (Grammar aa') = aa == aa'
    _ == _ = False

instance Eq RulesOrGrammarDeclaration where
    (RulesOrGrammarDeclaration_1 aa) ==
				     (RulesOrGrammarDeclaration_1 aa') = aa == aa'
    (RulesOrGrammarDeclaration_2 aa) ==
				     (RulesOrGrammarDeclaration_2 aa') = aa == aa'
    _ == _ = False

instance Eq Rules where
    (Rules aa ab) == (Rules aa' ab') = aa == aa' && ab == ab'
    _ == _ = False

instance Eq Rhses where
    (Rhses_1 aa) == (Rhses_1 aa') = aa == aa'
    (Rhses_2 aa ab) == (Rhses_2 aa' ab') = aa == aa' && ab == ab'
    (Rhses_3 aa) == (Rhses_3 aa') = aa == aa'
    _ == _ = False

instance Eq Rhs where
    (Rhs aa) == (Rhs aa') = aa == aa'
    _ == _ = False

instance Eq RhsSymb where
    (Symb aa) == (Symb aa') = aa == aa'
    (Code aa) == (Code aa') = aa == aa'
    (PrecDecl aa) == (PrecDecl aa') = aa == aa'
    (DPrecDecl aa) == (DPrecDecl aa') = aa == aa'
    (MergeDecl aa) == (MergeDecl aa') = aa == aa'
    _ == _ = False

instance Eq Symbol where
    (SymbolId aa) == (SymbolId aa') = aa == aa'
    (SymbolStrLex aa) == (SymbolStrLex aa') = aa == aa'
    (SymbolChar aa) == (SymbolChar aa') = aa == aa'
    _ == _ = False

--  Imported from other files :-
