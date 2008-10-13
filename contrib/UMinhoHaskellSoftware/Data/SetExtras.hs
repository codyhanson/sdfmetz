-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SetExtras
-- Copyright   :  (c) Alexandra Mendes, Jo�o Ferreira
-- License     :  GPL
--
-- Maintainer  :  Jo�o Ferreira, Alexandra Mendes
-- Stability   :  experimental
-- Portability :  experimental
--
-- Extra functions to use with Sets
-- 
-----------------------------------------------------------------------------


module Data.SetExtras (
-- * Sets' basic functions
   filterSet,
   dunion,

-- *  File IO
   readFile_Set,
   interact_Set
) where

import Data.Set

{-# DEPRECATED filterSet "Use Data.Set.filter instead." #-}
-- | Given a predicate p and a set, yields a set whose elements validate p.
filterSet :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet p s = fromList $ Prelude.filter (\key -> p key) $ elems s

-- | Given a set of sets ss, the resulting set is the union of all the elements
--   (these are sets themselves) of ss, i.e. it contains all the elements of all the 
--   sets of ss.
dunion :: Ord a => Set (Set a) -> Set a
dunion s = foldr (\s1 s2 -> union s1 s2) empty $ elems s

-- | Applies a given function to a set read from a given file.  
readFile_Set :: (Read a, Ord a, Show c) => FilePath -> ((Set a) -> c) -> IO c
readFile_Set file f = readFile file >>= return.f.read


-- | Applies readFile_Set and writes the result in a given file.
interact_Set :: (Read a, Ord a, Show c) =>
         FilePath -> FilePath -> ((Set a) -> c) -> IO ()
interact_Set inn out f = readFile_Set inn f >>= writeFile out.show

-- * Additional instances

{- Now available from Data.Set
instance Show a => Show (Set a) where
  show xs = "{" ++ show' (setToList xs) ++  "}"
            where
              show' []     = ""
              show' (x:xs) = show x ++ showl xs
              showl []     = ""
              showl (x:xs) = "," ++ show x ++ showl xs
-}
{- Produces:
Data/SetExtras.hs:61:0:
    Duplicate instance declarations:
      instance [overlap ok] (Ord a, Read a) => Read (Set a)
        -- Defined at Data/SetExtras.hs:61:0
      instance (Read a, Ord a) => Read (Set a) -- Defined in Data.Set

instance (Ord a, Read a) => Read (Set a) where
  readsPrec p = readSet'
-}
readSet'  :: (Ord a,Read a) => ReadS (Set a)
readSet' str = Prelude.map (\(a,b)->(fromList a,b)) (readSet str)

readSet   :: Read a => ReadS [a]
readSet   = readParen False (\r -> [pr | ("{",s) <- lex r,
                                         pr      <- readl s ])
            where readl  s = [([],t)   | ("}",t) <- lex s] ++
                             [(x:xs,u) | (x,t)   <- reads s,
                                         (xs,u)  <- readl' t]
                  readl' s = [([],t)   | ("}",t) <- lex s] ++
                             [(x:xs,v) | (",",t) <- lex s,
                                         (x,u)   <- reads t,
                                         (xs,v)  <- readl' u]

{- Now available from Data.Set
instance Ord a => Ord (Set a) where
  compare a b
    = compare (setToList a) (setToList b)  
-}
