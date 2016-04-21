-----------------------------------------------------------------------------
-- |
-- Maintainer  :  Tiago Alves and Joost Visser
-- Stability   :  experimental
-- Portability :  portable
--
-- Halstead metrics for Sdf grammars, taken from:
--   <http://mdp.ivv.nasa.gov/halstead_metrics.html>
--   <http://www.sei.cmu.edu/str/descriptions/halstead_body.html>
--
-----------------------------------------------------------------------------

module Language.Sdf.Metrics.Halstead where

import Data.Metrics
import StrategyLib
import Language.Sdf.SdfLib
import Data.Set (mkSet, cardinality)
import Control.Monad.Identity (runIdentity)
import Language.Sdf.Metrics.Size (calcTERM, calcUVar, calcSize)

-----------------------------------------------------------------------------
-- * Representation

-- | Data type to hold all metric results
data HalsteadMetrics = HalsteadMetrics
   { -- | Number of Distinct Operators
     u1 :: Int
     -- | Number of Distinct Operands  
   , u2 :: Int
     -- | Total Number of Operators
   , n1 :: Int
     -- | Total Number of Operands
   , n2 :: Int
     -- | Program Vocabulary (n = n1 + n2)
   , pn :: Int   
     -- | Program Length (N = N1 + N2)
   , pN :: Int
     -- | Program Volume (V = N * LOG2 n)
   , pV :: Float
     -- | Program Difficulty (D = (n1\/2) * (N2\/n2))
   , pD :: Float
     -- | Program Effort (E = D * V)
   , pE :: Float
     -- | Program Level (L = (2\/n1) * (n2\/N2))
   , pL :: Float
     -- | Programming Time (T = E\/18)
   , pT :: Float  
   }

-- | Creates a uniform representation of the metrics converting the internal
--   'HalsteadMetrics' record to a 'GroupMetric' type which holds not only all
--   the calculated metric values but also all information about the metrics:
--   description and name.
halsteadMetrics :: HalsteadMetrics -> GroupMetric
halsteadMetrics sm = 
   let u1M = Metric "Number of Distinct Operators"  "n1" (show $ u1 sm)
       u2M = Metric "Number of Distinct Operands"   "n2" (show $ u2 sm)
       n1M = Metric "Total Number of Operators"     "N1" (show $ n1 sm)
       n2M = Metric "Total Number of Operands"      "N2" (show $ n2 sm)
       pnM = Metric "Program Vocabulary"            "n"  (show $ pn sm)
       pNM = Metric "Program Length"                "N"  (show $ pN sm)
       pVM = Metric "Program Volume"                "V"  (show $ pV sm)
       pDM = Metric "Program Difficulty"            "D"  (show $ pD sm)
       pEM = Metric "Program Effort (in thousands)" "E"  (show $ pE sm)
       pLM = Metric "Program Level"                 "L"  (show $ pL sm)
       pTM = Metric "Programming Time"              "T"  (show $ pT sm) 
       metrics = [u1M,u2M,n1M,n2M,pnM,pNM,pVM,pDM,pEM,pLM,pTM]
   in GroupMetric "Halstead Metrics" metrics

-----------------------------------------------------------------------------
-- * Function to calculate all metrics

-- | Calculates all Halstead metrics
calcHalsteadMetrics :: SDF -> HalsteadMetrics
calcHalsteadMetrics sdf =
   let cfprods = (filter (not . isReject) . collectCfProductions) sdf
       (operators, operands) = calcOperatorsAndOperands cfprods
       u1 = cardinality $ mkSet operators
       u2 = cardinality $ mkSet operands
       n1 = length operators
       n2 = length operands
       pn = u1 + u2
       pN = n1 + n2
       pV = calcVolume pN pn
       pD = calcDifficulty u1 u2 n2
       pE = pD * pV
       pL = calcLevel u1 u2 n2
       pT = pE/18
   in HalsteadMetrics { u1 = u1, u2 = u2
                      , n1 = n1, n2 = n2
                      , pn = pn, pN = pN
                      , pV = pV
                      , pD = pD
                      , pE = (pE / 1000)
                      , pL = pL
                      , pT = pT 
                      }

-----------------------------------------------------------------------------
-- * Functions to separate Operators and Operands

-- | Calculates all operators and operands from a list of productions.
--   Each operator will be represented by its key (an unique number) and
--   for each operand its value (string) will be used.
calcOperatorsAndOperands :: [Production] -> ([Int],[String])
calcOperatorsAndOperands
   = (foldr worker ([],[])) . concatMap sepOperatorsAndOperands
    where worker (Left x)  (operators,operands) = (operators ++ [x], operands) 
          worker (Right x) (operators,operands) = (operators, operands ++ [x])


-- | Separate Operators and Operands from an SDF Production.
sepOperatorsAndOperands :: Production -> [Either Int String]
sepOperatorsAndOperands (Sdf_prod_fun lit symbs symb _)
   = [Left 1, Right (lit2string lit)] ++ (sepOaOfromLstSymbs symbs)
     ++ (sepOaOfromSymb symb)
sepOperatorsAndOperands (Sdf_prod (Sdf_list7 symbs) symb _)
   = [Left 1] ++ (sepOaOfromLstSymbs symbs) ++ (sepOaOfromSymb symb)


-- | Separate Operators and Operands of an SDF Symbol by recursively transverse
--   the Symbol data-type. For each operator returns its key (number) and for
--   each operand returns its value.
sepOaOfromSymb :: Symbol -> [Either Int String]
sepOaOfromSymb (Sdf_label lit symb)
   = [Left 2, Right (lit2string lit)] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_lit lit)
   = [Right (lit2string lit)] 
sepOaOfromSymb (Sdf_sort sort)
   = [Right sort]
sepOaOfromSymb (Sdf_char_class1 cc)
   = [Right (show cc)]
sepOaOfromSymb (Sdf_empty1)
   = [Left 3]
sepOaOfromSymb (Sdf_seq1 symb symbs)
   = [Left 4] ++ (sepOaOfromSymb symb) ++ (sepOaOfromLstSymbs symbs)
sepOaOfromSymb (Sdf_opt symb) 
   = [Left 5] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_iter symb)
   = [Left 6] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_iter_star symb) 
   = [Left 7] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_iter_sep symb1 symb2) 
   = [Left 8] ++ (sepOaOfromSymb symb1) ++ (sepOaOfromSymb symb2)
sepOaOfromSymb (Sdf_iter_star_sep symb1 symb2)
   = [Left 9] ++ (sepOaOfromSymb symb1) ++ (sepOaOfromSymb symb2)
sepOaOfromSymb (Sdf_iter_n symb nc)
   = [Left 10, Right nc] ++ (sepOaOfromSymb symb) 
sepOaOfromSymb (Sdf_iter_sep_n symb1 symb2 nc) 
   = [Left 11, Right nc] ++ (sepOaOfromSymb symb1) ++ (sepOaOfromSymb symb2)
sepOaOfromSymb (Sdf_set symb)
   = [Left 12] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_pair symb1 symb2) 
   = [Left 13] ++ (sepOaOfromSymb symb1) ++ (sepOaOfromSymb symb2)
sepOaOfromSymb (Sdf_func (Sdf_list7 symbs) symb) 
   = [Left 14] ++ (sepOaOfromSymb symb) ++ (sepOaOfromLstSymbs symbs)
sepOaOfromSymb (Sdf_alt1 symb1 symb2)
   = [Left 15] ++ (sepOaOfromSymb symb1) ++ (sepOaOfromSymb symb2)
sepOaOfromSymb (Sdf_perm (Sdf_list7 symbs))
   = [Left 16] ++ (sepOaOfromLstSymbs symbs)
sepOaOfromSymb (Sdf_cf symb)
   = [Left 17] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_lex symb)
   = [Left 18] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_varsym symb)
   = [Left 19] ++ (sepOaOfromSymb symb)
sepOaOfromSymb (Sdf_layout)
   = [Left 20]
sepOaOfromSymb (Sdf_start)
   = [Left 21]
sepOaOfromSymb (Sdf_file_start)
   = [Left 22]


-- | Separates the operators and operands from a list of symbols. This function
--   counts the "invisible" concatenation operator and invokes the 
--'sepOaOfromSymb' function to traverse the Symbol data type.
sepOaOfromLstSymbs :: [Symbol] -> [Either Int String]
sepOaOfromLstSymbs []  = []
sepOaOfromLstSymbs [a] = sepOaOfromSymb a
sepOaOfromLstSymbs (a : b : c)
   = [Left 0] ++ (sepOaOfromSymb a) ++ sepOaOfromLstSymbs (b:c)

-----------------------------------------------------------------------------
-- * Individual metric calculation functions

-- | Calculates the Halstead Program Volume
--   Halstead Program Volume is calculated by the following formula:
--
--   @ N * log2 nÊ@
--
-- where
--
--    * N = Program Length
--
--    * n = Program Vocabulary
calcVolume :: Int -> Int -> Float
calcVolume pN pn
   = if (pn == 0) then 0.0
      else (fromIntegral pN) * log2 (fromIntegral pn)


-- | Calculates the Halstead Program Difficulty
--   Halstead Program Difficulty is calculated by the following formula:
--
-- @ (u1 \/ 2) * (n2 \/ u2) @
--
-- where
--
--    * u1 = Number of Distinct Operators
--
--    * u2 = Number of Distinct Operands
--
--    * n2 = Total Number of Operands
calcDifficulty :: Int -> Int -> Int -> Float
calcDifficulty u1 u2 n2
   = if (u2 == 0) then 0.0
      else ((fromIntegral u1)/2) * ((fromIntegral n2)/(fromIntegral u2))


-- | Calculates the Halstead Program Level
--   Halstead Program Level is calculated by the following formula:
--
-- @ (2 \/ u1) * (u2 \/ n2)Ê@
--
-- where
--
--    * u1 = Number of Distinct Operators
--
--    * u2 = Number of Distinct Operands
--
--    * n2 = Total Number of Operands
calcLevel :: Int -> Int -> Int -> Float
calcLevel u1 u2 n2
   = if (u1 == 0) || (n2 == 0) then 0.0
      else (2/(fromIntegral u1)) * ((fromIntegral u2)/ (fromIntegral n2))

-----------------------------------------------------------------------------
-- * Auxiliary functions

-- | Helper function that calculates the log2 of a floating point number
log2 :: Floating a => a -> a
log2 x = (log x) / (log 2)

-----------------------------------------------------------------------------
