module Data.PartialOrder where

import Data.Set

class PartialOrd a where
  maybeCompare :: a -> a -> Maybe Ordering
  (.<=.) :: a -> a -> Bool
  x .<=. y = maybe False (/=GT) $ maybeCompare x y
  (.>.)  :: a -> a -> Bool
  x .>. y = maybe False (==GT) $ maybeCompare x y
  (.>=.) :: a -> a -> Bool
  x .>=. y = maybe False (/=LT) $ maybeCompare x y
  (.<.)  :: a -> a -> Bool
  x .<. y = maybe False (==LT) $ maybeCompare x y
  (.==.)  :: a -> a -> Bool
  x .==. y = maybe False (==EQ) $ maybeCompare x y
  (./=.)  :: a -> a -> Bool
  x ./=. y = maybe False (/=EQ) $ maybeCompare x y
  (.||.)  :: a -> a -> Bool
  x .||. y = maybe True (const False) $ maybeCompare x y

instance Ord a => PartialOrd (Set a) where
  maybeCompare x y = case (Data.Set.null xMy, Data.Set.null yMx) of
      (False,False) -> Nothing
      (True,False)  -> Just LT
      (False,True)  -> Just GT
      (True,True)   -> Just EQ
    where xMy = x `difference` y
          yMx = y `difference` x
    
-----------------------------------------------------------------------------


