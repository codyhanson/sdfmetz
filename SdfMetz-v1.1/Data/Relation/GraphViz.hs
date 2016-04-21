-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Universidade do Minho, 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
--- Print graphs in the dot format of GraphViz.
--
-----------------------------------------------------------------------------

module Data.Relation.GraphViz where

import Data.Set
import Data.Relation.SetOfPairs

-------------------------------------------------------------------------------
-- * Representation

-- | The type of dot statements.  
type DotStatement = String

-- | A list of dot attributes is represented by a string.
type DotAttributes = String

type NodeName = String

type GraphName = String

-------------------------------------------------------------------------------
-- * Printing

-- | Create a node statement
mkNode :: DotAttributes -> NodeName -> DotStatement
mkNode as ""        = "" 
mkNode as n         = quote n++" [ "++as++" ]" 

-- | Create an edge statement
mkEdge :: DotAttributes -> (NodeName,NodeName) -> DotStatement
mkEdge as ("",y)    = "" 
mkEdge as (x,y)     = quote x++" -> "++quote y++" [ "++as++" ]" 

-- | Create an edge statement with inverted direction
mkEdgeBack :: DotAttributes -> (NodeName,NodeName) -> DotStatement
mkEdgeBack as ("",y)    = "" 
mkEdgeBack as (x,y)     = quote y++" -> "++quote x++" [ "++as++" ]" 

-- * Auxilliaries

-- | Put quotes around a string.
quote :: String -> String
quote s = "\""++(esc s)++"\""
  where
    esc [] = []
    esc ('\"':cs) = '\\':'\"':esc cs
    esc (c:cs) = c:esc cs

-- | Create content for dot file representing the given relation.
printRel :: (Show a, Ord a, Show b, Ord b) 
           => GraphName -> Rel a b -> String
printRel graphName r
  = printRelWith show (const show) show (const show) graphName r
  
printGraphWith showNode showNodeLabel graphName
  = printRelWith showNode showNodeLabel showNode showNodeLabel graphName

printRelWith showInNode showInNodeLabel showOutNode showOutNodeLabel graphName r
  = "digraph "++graphName++" {\n"++
    unlines outNodeStats++
    unlines inNodeStats++
    unlines edgeStats++
    "}\n"
    where
      inNodeStats = map mkInNode (setToList (dom r))
      outNodeStats = map mkOutNode (setToList (rng r))
      edgeStats = map showEdge (setToList r)
      showEdge (x,y) = mkEdge "" (showInNode x, showOutNode y)
      mkInNode n = mkNode' ("shape=box,label="++ (quote (showInNodeLabel r n)))
                          (showInNode n)
      mkOutNode n = mkNode' ("shape=box,label="++ (quote (showOutNodeLabel r n)))
                           (showOutNode n)
      mkNode' "shape=box,label=\"\\n\"" n
        = mkNode "shape=circle,height=.2,label=\"\"" n
      mkNode' a n = mkNode a n

-------------------------------------------------------------------------------
