module Language.Bison.SyntaxPP where
import Language.Bison.Syntax
import Language.Bison.TermInstances
import Text.PrettyPrint.HughesPJ hiding (Mode)
import GPP
import StrategyLib
 
instance PP BisonSyntax where
        pp gpp (BisonSyntax _0 _2 _3)
          = fsep [gppList gpp _0, gpp "%%", gpp _2, gppMaybe gpp _3]
 
instance PP Declaration where
        pp gpp (GrammarDecl _0) = fsep [gpp _0]
        pp gpp (PrologueDecl _0) = fsep [gpp _0]
        pp gpp (DebugDecl) = fsep [gpp "%debug"]
        pp gpp (DefineDecl _1 _2) = fsep [gpp "%define", gpp _1, gpp _2]
        pp gpp (DefinesDecl) = fsep [gpp "%defines"]
        pp gpp (ErrorVerboseDecl) = fsep [gpp "%error-verbose"]
        pp gpp (ExpectDecl _1) = fsep [gpp "%expect", gpp _1]
        pp gpp (ExpectRRDecl _0 _1) = fsep [gpp _0, gpp _1]
        pp gpp (FilePrefixDecl _2)
          = fsep [gpp "%file-prefix", gpp "=", gpp _2]
        pp gpp (GlrParserDecl) = fsep [gpp "%glr-parser"]
        pp gpp (InitialActionDecl _1)
          = fsep [gpp "%initial-action", gpp _1]
        pp gpp (LexParamDecl _1) = fsep [gpp "%lex-param", gpp _1]
        pp gpp (LocationsDecl) = fsep [gpp "%locations"]
        pp gpp (NamePrefixDecl _0 _2) = fsep [gpp _0, gpp "=", gpp _2]
        pp gpp (NoLinesDecl _0) = fsep [gpp _0]
        pp gpp (NonDetParserDecl) = fsep [gpp "%nondeterministic-parser"]
        pp gpp (OutputDecl _2) = fsep [gpp "%output", gpp "=", gpp _2]
        pp gpp (ParseParamDecl _1) = fsep [gpp "%parse-param", gpp _1]
        pp gpp (PureParserDecl _0) = fsep [gpp _0]
        pp gpp (SkeletonDecl _1) = fsep [gpp "%skeleton", gpp _1]
        pp gpp (TokenTableDecl _0) = fsep [gpp _0]
        pp gpp (VerboseDecl) = fsep [gpp "%verbose"]
        pp gpp (YaccDecl) = fsep [gpp "%yacc"]
        pp gpp (SemiColon) = fsep [gpp ";"]
 
instance PP GrammarDeclaration where
        pp gpp (LeftPriorDecl _1 _2)
          = fsep [gpp "%left", gppMaybe gpp _1, gppList gpp _2]
        pp gpp (RightPriorDecl _1 _2)
          = fsep [gpp "%right", gppMaybe gpp _1, gppList gpp _2]
        pp gpp (BinaryPriorDecl _1 _2)
          = fsep [gpp "%binary", gppMaybe gpp _1, gppList gpp _2]
        pp gpp (NonAssocPriorDecl _1 _2)
          = fsep [gpp "%nonassoc", gppMaybe gpp _1, gppList gpp _2]
        pp gpp (NTermDecl _1) = fsep [gpp "%nterm", gppList gpp _1]
        pp gpp (TermDecl _1) = fsep [gpp "%term", gppList gpp _1]
        pp gpp (TokenDecl _0 _1) = fsep [gpp _0, gppList gpp _1]
        pp gpp (TypeDecl _1 _2)
          = fsep [gpp "%type", gpp _1, gppList gpp _2]
        pp gpp (StartDecl _1) = fsep [gpp "%start", gpp _1]
        pp gpp (UnionDecl _1) = fsep [gpp "%union", gpp _1]
        pp gpp (DestructorDecl _1 _2)
          = fsep [gpp "%destructor", gpp _1, gppList gpp _2]
        pp gpp (PrinterDecl _1 _2)
          = fsep [gpp "%printer", gpp _1, gppList gpp _2]
        pp gpp (DefaultPrecDecl _0) = fsep [gpp _0]
        pp gpp (NoDefaultPrecDecl _0) = fsep [gpp _0]
 
instance PP Type where
        pp gpp (Type _1) = fsep [gpp "<", gpp _1, gpp ">"]
 
instance PP SymbolDef where
        pp gpp (TypeDef _0) = fsep [gpp _0]
        pp gpp (IdDef _0) = fsep [gpp _0]
        pp gpp (IdIntDef _0 _1) = fsep [gpp _0, gpp _1]
        pp gpp (IdStrLexDef _0 _1) = fsep [gpp _0, gpp _1]
        pp gpp (IdIntStrLexDef _0 _1 _2) = fsep [gpp _0, gpp _1, gpp _2]
 
instance PP Grammar where
        pp gpp (Grammar _0) = fsep [gppList gpp _0]
 
instance PP RulesOrGrammarDeclaration where
        pp gpp (RulesOrGrammarDeclaration_1 _0) = fsep [gpp _0]
        pp gpp (RulesOrGrammarDeclaration_2 _0) = fsep [gpp _0, gpp ";"]
 
instance PP Rules where
        pp gpp (Rules _0 _2) = fsep [gpp _0, gpp ":", gpp _2]
 
instance PP Rhses where
        pp gpp (Rhses_1 _0) = fsep [gpp _0]
        pp gpp (Rhses_2 _0 _2) = fsep [gpp _0, gpp "|", gpp _2]
        pp gpp (Rhses_3 _0) = fsep [gpp _0, gpp ";"]
 
instance PP Rhs where
        pp gpp (Rhs _0) = fsep [gppList gpp _0]
 
instance PP RhsSymb where
        pp gpp (Symb _0) = fsep [gpp _0]
        pp gpp (Code _0) = fsep [gpp _0]
        pp gpp (PrecDecl _1) = fsep [gpp "%prec", gpp _1]
        pp gpp (DPrecDecl _1) = fsep [gpp "%dprec", gpp _1]
        pp gpp (MergeDecl _1) = fsep [gpp "%merge", gpp _1]
 
instance PP Symbol where
        pp gpp (SymbolId _0) = fsep [gpp _0]
        pp gpp (SymbolStrLex _0) = fsep [gpp _0]
        pp gpp (SymbolChar _0) = fsep [gpp _0]
 
uppSyntax :: UPP
uppSyntax gpp
  = (constTU empty) `adhocQ` (pp gpp :: MonoPP String) `adhocQ`
      (pp gpp :: MonoPP BisonSyntax)
      `adhocQ` (pp gpp :: MonoPP Declaration)
      `adhocQ` (pp gpp :: MonoPP GrammarDeclaration)
      `adhocQ` (pp gpp :: MonoPP Type)
      `adhocQ` (pp gpp :: MonoPP SymbolDef)
      `adhocQ` (pp gpp :: MonoPP Grammar)
      `adhocQ` (pp gpp :: MonoPP RulesOrGrammarDeclaration)
      `adhocQ` (pp gpp :: MonoPP Rules)
      `adhocQ` (pp gpp :: MonoPP Rhses)
      `adhocQ` (pp gpp :: MonoPP Rhs)
      `adhocQ` (pp gpp :: MonoPP RhsSymb)
      `adhocQ` (pp gpp :: MonoPP Symbol)
