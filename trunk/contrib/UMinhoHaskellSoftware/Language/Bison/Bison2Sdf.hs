------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Tiago Alves
-- Stability	: experimental
-- Portability	: portable
--
-- This module contains the convertion between Bison Grammars to SDF grammars.

------------------------------------------------------------------------------

module Language.Bison.Bison2Sdf (bison2sdf) where

import Language.Bison.Syntax as Bison
import Language.Bison.TermInstances
import Language.Sdf.SdfLib as Sdf

import Data.Map as Map
import Data.Set as Set
import StrategyLib


type TokenDefs = (Set String, Map String String)


-- | Converts a Bison grammar AST to an SDF grammar AST
bison2sdf :: BisonSyntax -> SDF
bison2sdf grammar =
   let lstTerms = Set.fromList $ concat $ declTerms grammar
       lstRedefs = Map.fromList $ concat $ listRedefs grammar
       rules = collectRules grammar
   in sdfHeader $ concatMap (rules2prods (lstTerms, lstRedefs)) rules
       

-- | Returns a list of all declared terminals.
declTerms :: Monad m => BisonSyntax -> m [String]
declTerms = applyTU (full_tdTU $ (constTU []) `adhocTU` worker)
   where worker :: Monad m => GrammarDeclaration -> m [String]
         worker (TokenDecl _ symbs) = return $ concatMap takeTerm symbs
         worker _                   = return []
         
         takeTerm :: SymbolDef -> [String]
         takeTerm (IdDef term)               = [term]
         takeTerm (IdIntDef term _)          = [term]
         takeTerm (IdStrLexDef term _)       = [term]
         takeTerm (IdIntStrLexDef term _ _ ) = [term]
         takeTerm (TypeDef _)                = []
   

-- | Returns a list of all redefinitions.
listRedefs :: Monad m => BisonSyntax -> m [(String,String)]
listRedefs = applyTU (full_tdTU $ (constTU []) `adhocTU` worker)
   where worker :: Monad m => GrammarDeclaration -> m [(String,String)]
         worker (TokenDecl _ symbs) = return $ concatMap getRedefs symbs
         worker _                   = return []
         
         getRedefs (IdStrLexDef term def)      = [(def,term)]
         getRedefs (IdIntStrLexDef term _ def) = [(def,term)]
         getRedefs _                           = []



-- | Collect all the rules from a Bison grammar.
collectRules :: BisonSyntax -> [Rules]
collectRules = concat . (applyTU (full_tdTU $ (constTU []) `adhocTU` worker))
   where worker x@(Rules _ _) = return [x]


-- | Converts a bison rule to a set of SDF productions. Although it alternatives
--   could be used in SDF, the use of several productions was preferred.
rules2prods :: TokenDefs -> Rules -> [Production]
rules2prods td (Rules nt rhses) =
   let rhs = rhses2symbs td rhses
       lhs = Sdf_sort nt
   in [(Sdf_prod (Sdf_list7 s) lhs Sdf_no_attrs) | s <- rhs ]


-- | Cycles through all the symbols of the right hand side of a Bison rule.
rhses2symbs :: TokenDefs -> Rhses -> [[Sdf.Symbol]]
rhses2symbs td (Rhses_1 rhs)       = [rhs2symbs td rhs]
rhses2symbs td (Rhses_2 rhses rhs) = (rhses2symbs td rhses) ++ [rhs2symbs td rhs]
rhses2symbs td (Rhses_3 rhses)     = rhses2symbs td rhses


-- | Converts all symbols for a single bison rule alternative.
rhs2symbs :: TokenDefs -> Rhs -> [Sdf.Symbol]
rhs2symbs td (Rhs symbs) = concatMap aux symbs
   where aux (Symb s) = [bisonSymb2SdfSymb td s]
         aux _        = []


-- | Converts bison symbols to its sdf equivalent.
bisonSymb2SdfSymb :: TokenDefs -> Bison.Symbol -> Sdf.Symbol
bisonSymb2SdfSymb tk (SymbolId s)
   = if (s `Set.member` (fst tk))
      then Sdf_lit $ Sdf_uqlit s
      else Sdf_sort s
bisonSymb2SdfSymb tk (SymbolStrLex s)
   = Sdf_lit $ Sdf_uqlit ((snd tk) ! s)
bisonSymb2SdfSymb _ (SymbolChar s)
   = Sdf_lit $ Sdf_quoted $ convertQuotes s


-- | Convert characters to quoted strings.         
convertQuotes :: String -> String
convertQuotes ('\'':str) = '\"':(convertQuotes' str)
   where convertQuotes' ['\''] = "\""
         convertQuotes' ('\\':'\'':str) = "\\\'" ++ convertQuotes' str
         convertQuotes' (x:str) = x:(convertQuotes' str)
         convertQuotes' []     = error $ "Single quote missing in end of string" ++ str
convertQuotes str = error $ "Single quote missing in beginning of string" ++ str





