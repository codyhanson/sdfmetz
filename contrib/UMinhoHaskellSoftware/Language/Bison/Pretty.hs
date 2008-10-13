module Language.Bison.Pretty (renderBison) where

import GPP
import Language.Bison.Syntax
import Language.Bison.SyntaxPP
import Text.PrettyPrint.HughesPJ
import TermRep


-- | Bison Pretty-printer.
renderBison :: Term a => a -> String
renderBison = renderFix uppSyntax
