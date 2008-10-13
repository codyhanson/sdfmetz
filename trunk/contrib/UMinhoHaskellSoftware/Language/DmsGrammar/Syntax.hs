module Language.DmsGrammar.Syntax where
 
data DmsSyntax = DmsSyntax [DmsRule]
 
data DmsRule = Other DmsSymbol Number
	     | Rule NonTerminal [DmsSymbol] (Maybe NonTerminal)
	     | Disambiguation DmsSymbol
 
data DmsSymbol = DmsNonTerminal NonTerminal
	       | DmsTerminal Terminal
 
type Number = String
 
type NonTerminal = String
 
type Terminal = String