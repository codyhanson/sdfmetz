module Data.Relation.Write where

import Data.Relation.GraphViz
import Data.Relation.SetOfPairs

-- | Create content for dot file representing the given relation.
printRel :: (Show a, Ord a, Show b, Ord b) 
           => GraphName -> Rel a b -> String
printRel graphName r
  = printRelWith show (const show) show (const show) graphName r
  
-----------------------------------------------------------------------------
