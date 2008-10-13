-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Universidade do Minho, 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
--- Print relations and graphs in the dot format of GraphViz.
--
-----------------------------------------------------------------------------

module Data.Relation.GraphViz where

import Data.Set hiding (map)
import Data.Relation.SetOfPairs 
import Data.Graph.Basics (Gph) 

-------------------------------------------------------------------------------
-- * Graph printing

-- | Print a graph to dot format. This function is parameterized 
--   with functions for printing names and labels of individual nodes.
printGraphWith 
  :: Ord a 
  => (a -> NodeName)              -- ^ print domain node identifier
  -> (Gph a -> a -> String)       -- ^ print domain node label
  -> GraphName                    -- ^ graph name
  -> Gph a                        -- ^ relation to print
  -> DotGraph                     -- ^ string to export to dot
printGraphWith showNode showNodeLabel graphName
  = printRelWith showNode showNodeLabel showNode showNodeLabel graphName

-- | Print a relation to dot format. This function is parameterized 
--   with functions for printing names and labels of individual nodes.
printRelWith 
  :: (Ord a, Ord b) 
  => (a -> NodeName)              -- ^ print domain node identifier
  -> (Rel a b -> a -> String)     -- ^ print domain node label 
  -> (b -> NodeName)              -- ^ print range node identifier
  -> (Rel a b -> b -> String)     -- ^ print range node label
  -> GraphName                    -- ^ graph name
  -> Rel a b                      -- ^ relation to print
  -> DotGraph                     -- ^ string to export to dot
printRelWith showInNode showInNodeLabel showOutNode showOutNodeLabel graphName r
  = "digraph "++graphName++" {\n"++
    unlines outNodeStats++
    unlines inNodeStats++
    unlines edgeStats++
    "}\n"
    where
      inNodeStats = map mkInNode (elems (dom r))
      outNodeStats = map mkOutNode (elems (rng r))
      edgeStats = map showEdge (elems r)
      showEdge (x,y) = mkEdge "" (showInNode x, showOutNode y)
      mkInNode n = mkNode' ("shape=box,label="++ (quote (showInNodeLabel r n)))
                          (showInNode n)
      mkOutNode n = mkNode' ("shape=box,label="++ (quote (showOutNodeLabel r n)))
                           (showOutNode n)
      mkNode' "shape=box,label=\"\\n\"" n
        = mkNode "shape=circle,height=.2,label=\"\"" n
      mkNode' a n = mkNode a n
      
-- | Print a graph to dot format. This function is parameterized 
--   with functions for printing names and labels of individual nodes.
printComponentGraphWith 
  :: Ord a 
  => (a -> NodeName)              -- ^ print identifier for element of set
  -> (a -> String)                -- ^ pretty-print element of set
  -> GraphName                    -- ^ graph name
  -> Gph (Set a)                  -- ^ relation to print
  -> DotGraph                     -- ^ string to export to dot
printComponentGraphWith showNodeName showNodeLabel graphName
  = printGraphWith showSet (\_ x -> showFormattedSet showNodeLabel x) graphName
    where
      showSet s    = show' $ elems s
      show' []     = ""
      show' (x:xs) = showNodeName x ++ showl xs
      showl []     = ""
      showl (x:xs) = "," ++ showNodeName x ++ showl xs

-- | Function that shows the elemens of a set spread nicely
--   over lines that are not too long. On each line elements
--   are separated by spaces. The number of elements of the
--   set determines how long each line is allowed to grow.
showFormattedSet 
  :: (a -> String)    -- ^ show function for elements
  -> Set a            -- ^ set to show
  -> String 
showFormattedSet showNode xs = shows 0 lst
  where
    lst = elems xs
    max = Data.Set.size xs
    mx = maximum (max:(map (length.showNode) lst))
    shows l []      = ""
    shows 0 (x:xs)  = wrd++(shows (length wrd) xs) where wrd = showNode x
    shows l (x:xs) 
      | l' > mx   = '\\':'n':(shows 0 (x:xs))
      | otherwise  = ' ':wrd++(shows l' xs)
      where 
        l' = l+(length wrd)+1
        wrd = showNode x

-------------------------------------------------------------------------------
-- * Representation

-- | The type of dot statements.  
type DotStatement = String

-- | A list of dot attributes is represented by a string.
type DotAttributes = String

-- | The type of node names
type NodeName = String

-- | The type of graph names
type GraphName = String

-- | The type of exportable graphs (file content)
type DotGraph = String

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

-------------------------------------------------------------------------------
