module Language.DmsGrammar.SyntaxPP where
import Language.DmsGrammar.Syntax
import Language.DmsGrammar.TermInstances
import Text.PrettyPrint.HughesPJ hiding (Mode)
import GPP
import StrategyLib
 
instance PP DmsSyntax where
	pp gpp (DmsSyntax _0) = fsep [gppList gpp _0]
 
instance PP DmsRule where
	pp gpp (Other _0 _2) = fsep [gpp _0, gpp "=", gpp _2, gpp ";"]
	pp gpp (Rule _0 _2 _4)
	  = fsep [gpp _0, gpp "=", gppList gpp _2, gpp ";", gppMaybe gpp _4]
	pp gpp (Disambiguation _1) = fsep [gpp ">>", gpp _1]
 
instance PP DmsSymbol where
	pp gpp (DmsNonTerminal _0) = fsep [gpp _0]
	pp gpp (DmsTerminal _0) = fsep [gpp _0]
 
uppDmsGrammarSyntax :: UPP
uppDmsGrammarSyntax gpp
  = (constTU empty) `adhocQ` (pp gpp :: MonoPP String) `adhocQ`
      (pp gpp :: MonoPP DmsSyntax)
      `adhocQ` (pp gpp :: MonoPP DmsRule)
      `adhocQ` (pp gpp :: MonoPP DmsSymbol)
