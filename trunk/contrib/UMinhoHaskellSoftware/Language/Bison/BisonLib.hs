------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Tiago Alves
-- Stability	: experimental
-- Portability	: portable
--
-- This module contains the library for Bison Grammar Syntax processing.

------------------------------------------------------------------------------

module Language.Bison.BisonLib
   ( module Language.Bison.BisonLib
   , module Language.Bison.Syntax
   , module Language.Bison.SyntaxPP
   , module Language.Bison.Pretty
   , module Language.Bison.EqInstances
   , module Language.Bison.ShowInstances
   , module Language.Bison.TermInstances
   , module Language.Bison.ATermInstances
   , module Language.Bison.Bison2Sdf
   ) where

import Language.Bison.Syntax
import Language.Bison.SyntaxPP
import Language.Bison.Pretty
import Language.Bison.EqInstances
import Language.Bison.ShowInstances
import Language.Bison.TermInstances
import Language.Bison.ATermInstances
import Language.Bison.Bison2Sdf

import System.Environment (getEnv)
import System.Directory (doesFileExist)


-- | Reads the environment variable BISON_SGLRTBL for the location of the
--   parse table required by sglr. If the variable is not set it fails with a
--   message error.
getBisonTable :: IO String
getBisonTable
  = do catch ( do tbl <- getEnv "BISON_SGLRTBL"
                  return tbl)
             ( const (fail "Environment variable 'BISON_SGLRTBL' is not set!"))
