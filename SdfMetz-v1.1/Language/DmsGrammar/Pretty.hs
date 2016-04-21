
module Language.DmsGrammar.Pretty (renderDms) where

import GPP
import Language.DmsGrammar.Syntax
import Language.DmsGrammar.SyntaxPP
import Text.PrettyPrint.HughesPJ


renderDms :: DmsSyntax -> String
renderDms = renderFix uppDmsGrammarSyntax