-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Metrics regarding the size of Sdf grammars.
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.Size where

import Data.Metrics
import StrategyLib
import Language.Sdf.SdfLib
import Pointless.Combinators
import Control.Monad
import Data.Set (mkSet, cardinality, intersect)
import Control.Monad.Identity (runIdentity)
import Data.FiniteMap (emptyFM, addToFM_C, foldFM)

-----------------------------------------------------------------------------
-- * Representation

-- | Data type to hold all metric results
data SizeMetrics = SizeMetrics
   { -- | Number of unique used non-terminals.
     term :: Float
     -- | Number of defined terminals.
   , var  :: Float
     -- | Number of used terminals.
   , uvar :: Float
     -- | Number of context-free productions
   , prod :: Float  
     -- | McCabe cyclomatic complexity. Measures the number of linearly
     -- independent paths through a flow graph.
   , mcc  :: Float
     -- | Average RHS per non-terminal. Calculated by the quotient of 'size' 
     --   and 'var'.
   , avsN  :: Float
     -- | Average RHS per production. Calculated by the quotient of 'size' and
     --   'prod'.
   , avsP  :: Float  
   }
   
-- | Creates a uniform representation of the metrics converting the internal
--   'SizeMetrics' record to a 'GroupMetric' type which holds not only all the
--   calculated metric values but also all information about the metrics:
--   description and name.
sizeMetrics :: SizeMetrics -> GroupMetric
sizeMetrics sm = 
   let termM = Metric "Number of unique terminals" "TERM" (show $ term sm)
       varM  = Metric "Number of defined non-terminals" "VAR" (show $ var sm)
       uvarM = Metric "Number of used non-terminals" "UVAR" (show $ uvar sm)
       prodM = Metric "Number of productions" "PROD" (show $ prod sm)
       mccM  = Metric "Cyclometric Complexity McCabe" "MCC" (show $ mcc sm)
       avsnM = Metric "Average RHS per non-terminal" "AVSN" (show $ avsN sm)
       avspM = Metric "Average RHS per production" "AVSP" (show $ avsP sm)
       metrics = [termM, varM, uvarM, prodM, mccM, avsnM, avspM]
   in GroupMetric "Size and Complexity Metrics" metrics

-----------------------------------------------------------------------------
-- * Function to calculate all metrics

-- | Calculates all size metrics
calcSizeMetrics :: SDF -> SizeMetrics
calcSizeMetrics sdf =
   let cfprods = (filter (not . isReject) . collectCfProductions) sdf
       var  = calcVAR cfprods
       prod = (fromIntegral . length) cfprods
       size = calcSize cfprods
   in SizeMetrics { term = calcTERM sdf
                  , var  = var
                  , uvar = calcUVar cfprods
                  , prod = prod
                  , mcc  = calcMCC cfprods
                  , avsN = calcAvsN size var
                  , avsP = calcAvsP size prod
                  }

-----------------------------------------------------------------------------
-- * Individual metric calculation functions

-- | Calculates the number of unique of terminals used in a list of productions.
{-
calcTERM :: [Production] -> Float
calcTERM = fromIntegral . cardinality . mkSet . 
              (map lit2string) . collectLiterals . (map getRHSsymbols)
-}

calcTERM :: SDF -> Float
calcTERM sdf = fromIntegral $ (cardinality literals) + (cardinality usedLexSorts)
   where literals     = mkSet $ collectTerminals rhsSymbs
         defLexSorts  = mkSet $ concatMap (getSortName . getSort) $
                        collectLexProductions sdf
         usedSorts    = mkSet $ collectSortNames rhsSymbs
         usedLexSorts = defLexSorts `intersect` usedSorts
         rhsSymbs     = map getRHSsymbols cfprods
         cfprods      = (filter (not . isReject) . collectCfProductions) sdf


collectTerminals :: Term a => a -> [Symbol]
collectTerminals = head . applyTU strategy
   where strategy = full_tdTU $ (constTU []) `adhocTU` getTerminal
         getTerminal :: Monad m => Symbol -> m [Symbol]
         getTerminal x@(Sdf_lit _)         = return [x]
         getTerminal x@(Sdf_char_class1 _) = return [x]
         getTerminal x@(Sdf_lex _)         = return [x]
         getTerminal _                     = return []

-- | Calculates the number of defined non-terminals in a list of productions
calcVAR :: [Production] -> Float
calcVAR = fromIntegral . cardinality . mkSet . concatMap (getSortName . getSort)


-- | Calculates the number of unique used non-terminals in a list of production.
--   This number can differ from calcVAR because a regular expression in a
--   grammar is used as a non-terminal but is not defined in contex-free syntax.
calcUVar :: [Production] -> Float
calcUVar = fromIntegral . cardinality . mkSet . 
              collectSortNames . map getRHSsymbols


-- | Function that calculates the McCabe cyclomatic complexity
calcMCC :: [Production] -> Float
calcMCC = uncurry (+) . (calcMCC1 /\ calcMCC2)
         
calcMCC1 :: [Production] -> Float
calcMCC1 = runIdentity . applyTU strategy
   where strategy = full_tdTU $ (constTU 0) `adhocTU` (return . sdfMcCabe)
         sdfMcCabe x = case x of
            (Sdf_opt _)              -> 1 -- "?"
            (Sdf_iter _)             -> 1 -- "+"
            (Sdf_iter_star _)        -> 1 -- "*"
            (Sdf_iter_sep _ _ )      -> 1 -- "{" .. "}" "+"
            (Sdf_iter_star_sep _ _ ) -> 1 -- "{" .. "}" "*"
            (Sdf_iter_n _ _)         -> 1 -- N "+"
            (Sdf_iter_sep_n _ _ _)   -> 1 -- "{" .. "}" N "+"
            (Sdf_alt1 _ _)           -> 1 -- "|"
            _                        -> 0

calcMCC2 :: [Production] -> Float
calcMCC2 = 
   let -- calculates a bag for the defined sorts. This will allow to know
       -- how many rules are associated with which non-terminal
       bagAritySorts = foldr (\e fm -> addToFM_C (+) fm e 1) emptyFM
       -- calculates the number of the alternative paths that each non-terminal
       -- define (number of the rules minus 1). 
       -- Here we calculate the sum of all alternatives 
       calcMcCabe = foldFM (\_ e a -> (a + e - 1)) 0
   in calcMcCabe . bagAritySorts . concatMap (getSortName . getSort)


-- | Calculates the Average RHS size per defined non-terminal. This value is
--   computed by dividing the total sum of the RHS (of all rules) by the number
--   of non-terminals.
calcAvsN :: Float -> Float -> Float
calcAvsN sumOpnds var = if (var <= 0) then 0 else (sumOpnds / var)


-- | Calculates the Average RHS size per production. This value is computed
--   by dividing the total sum of the RHS size (of all rules) by the number of
--   productions.
calcAvsP :: Float -> Float -> Float
calcAvsP sumOpnds prods = if(prods <= 0) then 0 else (sumOpnds / prods)


-- | Calculates the total RHS size of a grammar
calcSize :: [Production] -> Float
calcSize = runIdentity . applyTU strategy . (map getSyms)
   where strategy = full_tdTU $ (constTU 0) `adhocTU` (return . ruleSize)
         
         ruleSize (Sdf_lit _)         = 1
         ruleSize (Sdf_sort _)        = 1
         ruleSize (Sdf_char_class1 _) = 1
         ruleSize _                   = 0

-----------------------------------------------------------------------------
-- * Auxiliary functions

-- | Function that collect all literals of a term.
collectLiterals :: Term a => a -> [Literal]
collectLiterals = head . applyTU strategy
   where strategy = full_tdTU $ (constTU []) `adhocTU` getLiteral
         getLiteral = (\x -> return [x] :: MonadPlus m => m [Literal])


-- | Function that returns the RHS symbols of a production. This function is
--   a little more general that getSyms (SdfLib module) because it also returns
--   the literal from Sdf_prod_fun. It seems that some rules are recognized as
--   Sdf_prod_fun.
getRHSsymbols :: Production -> [Symbol]
getRHSsymbols (Sdf_prod_fun lit symbs _ _)     = (Sdf_lit lit):symbs
getRHSsymbols (Sdf_prod (Sdf_list7 symbs) _ _) = symbs

-----------------------------------------------------------------------------
