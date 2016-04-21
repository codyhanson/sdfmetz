{-----------------------------------------------------------------------------

	          A LIBRARY OF FUNCTIONAL STRATEGY COMBINATORS

		                  StrategyLib

                   Ralf Laemmel                Joost Visser
               CWI & VU, Amsterdam          CWI & SIG, Amsterdam

This module is part of a library of functional strategy combinators,
including combinators for generic traversal. This module defines
combinators to define metrics.

-----------------------------------------------------------------------------} 



module MetricsTheme where

import Monad
import Monoid
import StrategyPrelude
import OverloadingTheme
import FlowTheme



--- An abstract datatype for metrics  ----------------------------------------

type Metrics 		=  String -> Integer
initMetrics		:: Integer -> Metrics
initMetrics n       	=  \key -> n
initMetrics0  		=  initMetrics 0
initTypeMetrics key _   =  incMetrics1 key initMetrics0
incMetrics key n m 	=  \key' -> let val = m key' 
                                    in if key'==key then val+n else val
incMetrics1 key m 	=  incMetrics key 1 m
putMetricLn key m 	=  putStrLn $ key++" = "++show (m key)



--- Metrics as monoids -------------------------------------------------------

instance Monoid Metrics where
  mempty        = initMetrics0
  mappend m1 m2 = \s -> (m1 s) + (m2 s) 



--- Strategy combinator for type-based metrics -------------------------------

typeMetric s (key,g)
  = op2TU mappend
          (tryTU (ifthenTU (voidTU (typeFilterTU g))
			   (constTU (incMetrics1 key initMetrics0)))) 
          (tryTU s)



--- Strategy combinator for predicate-based metrics --------------------------

predMetric s (key,g)
  = op2TU mappend 
          (tryTU (ifthenTU (monoTU g)
                           (constTU (incMetrics1 key initMetrics0))))
          (tryTU s)



--- Generic algorithm for nesting depth --------------------------------------

depthWith :: MonadPlus m => TU () m -> TU Int m
depthWith s		
  =  allTU' ((:[]) `dotTU` depthWith s) `passTU` \ds ->
     let max_d = maximum (0:ds)
     in  (s `passTU` \() -> constTU (max_d + 1))
         `choiceTU`
         (constTU max_d)



-------------------------------------------------------------------------------
