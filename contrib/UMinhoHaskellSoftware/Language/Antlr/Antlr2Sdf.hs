
module Language.Antlr.Antlr2Sdf where

import Language.Antlr.Syntax as Antlr
import Language.Antlr.ShowInstances
import Language.Antlr.TermInstances
import Language.Antlr.ATermInstances
import Language.Sdf.SdfLib as Sdf

import Control.Monad.Identity (runIdentity)
import StrategyLib


antlr2sdf :: Antlr.Grammar -> SDF
antlr2sdf ag = sdfHeader $ map a2sRule (collectCfRules ag)


-- | Collect all defined context-free rules
collectCfRules :: Antlr.Grammar -> [Rule]
collectCfRules
  = runIdentity . applyTU (full_tdTU $ (constTU []) `adhocTU` worker)
    where
      worker cf@(ClassDef _ _ _ rules) | isContextFree cf = return rules
                                       | otherwise        = return []


a2sRule :: Rule -> Production
a2sRule (Rule _ _ id _ _ _ _ _ _ block _)
  = Sdf_prod (Sdf_list7 symbs) nt Sdf_no_attrs
    where nt    = a2sId id
          symbs =  case symb of
                    (Sdf_seq1 s ss) -> s:ss
                    otherwise      -> [symb]
          symb  = a2sBlock block


a2sId :: Id -> Symbol
a2sId (TokenRefId x) = Sdf_sort x
a2sId (RuleRefId  x) = Sdf_sort x


-- | Look to Spec header to know if it is a context-free
isContextFree :: ClassDef -> Bool
isContextFree (ClassDef _ _ 
                 (Spec (ClassHeader _ (TokenRefId "Parser") _ ) _ _ _)
              _) = True
isContextFree _ = False



a2sBlock :: Block -> Symbol
a2sBlock (Block [])    = Sdf_empty1
a2sBlock (Block [alt]) = a2sAlternative alt
a2sBlock (Block alts)
  = foldr1 Sdf_alt1 $ map a2sAlternative alts


a2sAlternative :: Alternative -> Symbol
a2sAlternative (Alternative _ elts _ )
 = a2s $ foldr (\a b -> maybe b (:b) a) [] (map  a2sElement elts)
  where
   a2s [] = Sdf_empty1
   a2s [sym] = sym
   a2s (sym:syms) = Sdf_seq1 sym syms


a2sElement :: Element -> Maybe Symbol
a2sElement (Element elt _) = a2sElementNoOptionSpec elt


a2sElementNoOptionSpec :: ElementNoOptionSpec -> Maybe Symbol
a2sElementNoOptionSpec (AssignElement _ _ as)
   = Just $ a2sAssignElementBody as
a2sElementNoOptionSpec (NoAssignElement _ na)
   = a2sNoAssignElementBody na
a2sElementNoOptionSpec (TreeElement tree) = undefined
a2sElementNoOptionSpec _ = Nothing


a2sAssignElementBody :: AssignElementBody -> Symbol
a2sAssignElementBody (RuleRefElt rr _ _) = Sdf_sort rr
a2sAssignElementBody (TokenRefElt tr _) = Sdf_lit $ Sdf_quoted tr


a2sNoAssignElementBody :: NoAssignElementBody -> Maybe Symbol
a2sNoAssignElementBody (RuleRefEltNoAssign rr _ _) = Just $ Sdf_sort rr
a2sNoAssignElementBody (RangeElt _ )
   = Just $ Sdf_char_class1 $ Sdf_simple_charclass $ Sdf_absent1
a2sNoAssignElementBody(TerminalElt t) = Just $ a2sTerminal t
a2sNoAssignElementBody (NegatedElt ne) = error ("Negated: "++(show ne))
a2sNoAssignElementBody (EbnfElt e) = a2sEbnf e



a2sTerminal :: Terminal -> Symbol
a2sTerminal (CharLiteralT x _ ) = Sdf_lit $ Sdf_quoted x
a2sTerminal (TokenRefT tr _ _) = Sdf_lit $ Sdf_uqlit tr
a2sTerminal (StringLiteralT st _) = Sdf_lit $ Sdf_quoted st
a2sTerminal (WildCardT _) 
   = Sdf_char_class1 $ Sdf_comp $ Sdf_simple_charclass $ Sdf_absent1



a2sEbnf :: Ebnf -> Maybe Symbol
a2sEbnf (Ebnf _ block body)
  = case body of
      (EbnfBodyRegExp (Just (Question _)) _) -> Just $ Sdf_opt sym
      (EbnfBodyRegExp (Just (Star _)) _)     -> Just $ Sdf_iter_star sym
      (EbnfBodyRegExp (Just (Plus _)) _)     -> Just $ Sdf_iter sym
      (EbnfBodyRegExp Nothing _) -> Just $ sym
      (EbnfBodyImplies _) -> Nothing
    where
      sym = a2sBlock block

