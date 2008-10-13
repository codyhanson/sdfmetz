-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- NPath metrics for Sdf grammars, taken from the paper: 
-- "NPath: A measure of execution path complexity and its applications" by
-- Brian A. Nejmeh
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.NPath where

import Data.Metrics
import Data.Map as Map hiding (map, filter)
import Language.Sdf.SdfLib


-----------------------------------------------------------------------------
-- * Representation

-- | Data type to hold all metric results
data NPathMetrics = NPathMetrics
   { -- | Total NPath
     npath :: Int 
   , -- | NPath per productuction
     npathp :: Float
   , -- | NPath per non-terminal
     npathn :: Float
   , -- | Maximum NPath per production
     maxnpathp :: Int
   , -- | Maximum NPath per non-terminal
     maxnpathn :: Int  
   }


-- | Creates a uniform representation of the metrics converting the internal
--   'NPathMetrics' record to a 'GroupMetric' type which holds not only all
--   the calculated metric values but also all information about the metrics:
--   description and name.
npathMetrics :: NPathMetrics -> GroupMetric
npathMetrics sm = 
   let metrics = [ Metric "NPath (total)" "NPATH" (show $ npath sm)
                 , Metric "NPath per production" "NPATHp" (show $ npathp sm)
                 , Metric "NPath per non-terminal" "NPATHn" (show $ npathn sm)
                 , Metric "Maximum NPath per production" "MaxNPATHp" (show $ maxnpathp sm)
                 , Metric "Maximum NPath per non-terminal" "MaxNPATHn" (show $ maxnpathn sm)
                 ]
   in GroupMetric "NPath metrics" metrics    

-----------------------------------------------------------------------------
-- * Function to calculate all metrics

-- | Calculates average NPath metric per productuction rule and per non-terminal
calcNPathMetrics :: SDF -> NPathMetrics
calcNPathMetrics sdf =
   let npathMap = calcNPath sdf
       (nt, product, total) = aggregateFmValues npathMap
       (maxNT, maxRules)    = maxFmValues npathMap
   in NPathMetrics { npath = total
                   , npathp = (fromIntegral total) / (fromIntegral product)
                   , npathn = (fromIntegral total) / (fromIntegral nt)
                   , maxnpathp = maxRules
                   , maxnpathn = maxNT
                   }


-- | Aggregate values from the Map to a pair containing the number of
--   non-terminals, the number of production rules, and the sum of the NPath
--   metric.
aggregateFmValues :: Map String [Int] -> (Int, Int, Int)
aggregateFmValues = Map.fold f (0,0,0)
   where f lst (nt, rules, total) = (nt+1, rules + length lst, total + sum lst)


-- | ...
maxFmValues :: Map String [Int] -> (Int, Int)
maxFmValues = Map.fold f (0, 0)
   where f lst (nt, rules) = (max (sum lst) nt, max (maximum lst) rules)


-- | Calculates the NPath metric for all production rules. It returns a
--   FiniteMap in which the key is the defined non-terminal and the range is a
--   list of the NPath metric for each production rule
calcNPath :: SDF -> Map String [Int]
calcNPath sdf = foldr calcNPathProduction empty cfrules
   where cfrules = (filter (not . isReject) . collectCfProductions) sdf


-- | Apply the NPath metric to the different SDF production rule types
calcNPathProduction (Sdf_prod_fun _ symbs nt _)       fm = applyNPath nt symbs fm
calcNPathProduction (Sdf_prod (Sdf_list7 symbs) nt _) fm = applyNPath nt symbs fm

    
-- | Apply the NPath metric to the production rule symbols
applyNPath :: Symbol -> [Symbol] -> Map String [Int] -> Map String [Int]
applyNPath s ss fm = addMetric fm (sortName s) (product $ map symbolNPath ss)


-- | Recursive function that calculates the NPath of an SDF Symbol
symbolNPath :: Symbol -> Int
symbolNPath (Sdf_label _ s)                = symbolNPath s
symbolNPath (Sdf_lit _)                    = 1
symbolNPath (Sdf_sort _)                   = 1
symbolNPath (Sdf_char_class1 _)            = 1
symbolNPath (Sdf_empty1)                   = 1
symbolNPath (Sdf_seq1 s symbs)             = (symbolNPath s) * 
                                             (product (map symbolNPath symbs))
symbolNPath (Sdf_opt s)                    = 1 + (symbolNPath s)
symbolNPath (Sdf_iter s)                   = symbolNPath s
symbolNPath (Sdf_iter_star s)              = 1 + (symbolNPath s)
symbolNPath (Sdf_iter_sep s1 s2)           = (symbolNPath s1) * 
                                             (1 + (symbolNPath s2))
symbolNPath (Sdf_iter_star_sep s1 s2)      = 1 + (symbolNPath s1) * 
                                             (1 + (symbolNPath s2))
symbolNPath (Sdf_iter_n s n)               = (symbolNPath s) ^ (read n)
symbolNPath (Sdf_iter_sep_n s1 s2 n)       = ((symbolNPath s1) ^ (read n)) * 
                                             ((symbolNPath s2) ^ ((read n) -1))
symbolNPath (Sdf_set s)                    = 1 + symbolNPath s         
symbolNPath (Sdf_pair s1 s2)               = (symbolNPath s1) * (symbolNPath s2)
symbolNPath (Sdf_func (Sdf_list7 symbs) s) = (product $ map symbolNPath symbs) *
                                             (symbolNPath s)
symbolNPath (Sdf_alt1 s1 s2)               = (symbolNPath s1) + (symbolNPath s2)
symbolNPath (Sdf_perm (Sdf_list7 symbs))   = product $ map symbolNPath symbs
symbolNPath (Sdf_cf s)                     = symbolNPath s
symbolNPath (Sdf_lex s)                    = symbolNPath s
symbolNPath (Sdf_varsym s)                 = symbolNPath s
symbolNPath (Sdf_layout)                   = 1
symbolNPath (Sdf_start)                    = 1
symbolNPath (Sdf_file_start)               = 1


-----------------------------------------------------------------------------
-- * Auxiliary functions

-- | Add metric value to the finite map.
addMetric :: Map String [Int] -> String -> Int -> Map String [Int]
addMetric fm nonterminal value = case (Map.lookup nonterminal fm) of
    (Just x) -> insert nonterminal (x ++ [value]) fm
    Nothing  -> insert nonterminal [value] fm


-- | Get the non-terminal name
sortName :: Symbol -> String
sortName (Sdf_sort x) = x
sortName _            = ""

