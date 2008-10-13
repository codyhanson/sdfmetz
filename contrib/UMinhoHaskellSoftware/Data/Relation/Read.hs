-----------------------------------------------------------------------------
-- |
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- Reading relations from files.
--
-----------------------------------------------------------------------------

module Data.Relation.Read where

import Data.Relation.SetOfPairs 
import Data.Set hiding (map)

-----------------------------------------------------------------------------
-- * Parsing spreadsheet-like input files.

readMatrix :: Char -> Int -> FilePath -> IO [[String]]
readMatrix sepChar n fileName
  = do content <- readFile fileName
       let ls = take n (lines content)
       return (map stripTokens ls)
    where  
      stripTokens
        = map (unwords . words) . tokenizeBy (sepChar==)

-- | Tokenize an input lines by a given separator character predicate.
tokenizeBy :: (Char -> Bool) -> String -> [String]
tokenizeBy p "" = []
tokenizeBy p s    
 = let (l,s') = break p s
   in l : case s' of []      -> []
                     (_:s'') -> tokenizeBy p s''

-----------------------------------------------------------------------------
-- * Convert to relations.

-- | Convert a matrix to a relation. Every cell gives rise to a pair with 
--   first element the key from the first column in the corresponding
--   row, and with second element a pair of the cell value and the column
--   number. Cells with null values do not give rise to any pair.
matrixToRel :: [[String]] -> Rel String (String, Int)
matrixToRel mx
  = foldr (\row rel -> rowToRel rel row) emptyRel mx

rowToRel :: Rel String (String, Int) 
         -> [String] 
         -> Rel String (String, Int)
rowToRel rel (h:t)
  = foldr (addPair h) rel (zip t [1..])
    where
      addPair x y@(cell,n) rel
        | isNullValue cell
        = rel
        | otherwise
        = insert (x,y) rel
rowToRel rel _ = rel

isNullValue "0" = True
isNullValue ""  = True
isNullValue "." = True
isNullValue _   = False

-----------------------------------------------------------------------------
-- * Auxiliaries

type FilterMask = [Bool]

-- | Keep or discard element of a list, depending on the truth value at the
--   corresponding position in a given filter mask. If the mask is shorter
--   than the given list, the list is truncated.
filterWithMask :: FilterMask -> [x] -> [x]       
filterWithMask mask l
  = concatMap decide (zip mask l)
    where
      decide (True,x) = [x]
      decide (_,_)    = []



-- | Compute the sets of values that occur in each collumn of a given
--   matrix.
valueSets :: Ord a => [Set a] -> [[a]] -> [Set a]
valueSets sets [] = sets
valueSets sets (r:rows) = valueSets (insrt r sets) rows
  where
    insrt [] sets = sets
    insrt row [] = map singleton row
    insrt (c:cells) (s:sets) = (insert c s) : insrt cells sets

-----------------------------------------------------------------------------
