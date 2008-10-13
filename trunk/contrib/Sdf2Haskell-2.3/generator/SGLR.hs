------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Ralf Laemmel, Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating a set of
-- Haskell data types from an SDF grammar. This module provides functionality
-- for calling the external parser SGLR from within Haskell and marshalling
-- the resulting abstract syntax tree to a strongly-typed Haskell term.

------------------------------------------------------------------------------

module SGLR where

import System (ExitCode(..),getProgName)
import System.Cmd (system)
import Data.Unique (newUnique, hashUnique)
import System.IO (readFile,hPutStrLn,stderr)
import ATermLib (fromATerm, readATerm, ATermConvertible, 
                 dehyphenAST, afunCap, ATerm(..)
                )
import ImplodePT (compensateImplodePT)

------------------------------------------------------------------------------
-- * Interaction with the system.

-- | Call the external sglr parser.
sglr :: ATermConvertible a
     => FilePath	-- ^ table
     -> FilePath	-- ^ term
     -> String          -- ^ top sort
     -> IO a
sglr tableName termName sortName
  = do unique <- newUnique
       let newName = termName++(show . hashUnique $ unique)
       let asfixName = newName++".asfix"
       let afName = newName++".af"
       let sglrCmd = "sglr -p "++tableName++
                         " -i "++termName++
			 " -o "++asfixName++
			 " -s "++sortName
       let implodeCmd = "implodePT -t -ALclpqX"++ 
                                 " -i "++asfixName++
				 " -o "++afName
       progName <- getProgName
       errLn $ "["++progName++"] "++sglrCmd			 
       exitOK $ system sglrCmd
       errLn $ "["++progName++"] "++implodeCmd
       exitOK $ system implodeCmd
       af <- readFile afName
       --return . fromATerm . afunCap . implodeLit . readATerm . dehyphen $ af
       --let t1 = readATerm . dehyphen $ af
       let t1 = dehyphenAST . readATerm $ af
       --putStrLn $ "After dehyphen: \n"++show t1
       let t2 = compensateImplodePT t1
       --putStrLn $ "After implodeOpt: \n"++show t2
       let t3 = afunCap t2
       return . fromATerm $ t3


-- | Helper function for reporting errors and progress to stderr
errLn :: String -> IO ()
errLn str = hPutStrLn stderr str

-- | Ensure that the given computation exits with an error code
--   that indicates successful execution.       
exitOK :: IO ExitCode -> IO ()
exitOK io
  = do exitCode <- io
       case exitCode of
         (ExitSuccess)   -> return ()
	 (ExitFailure code) -> fail $ "Exit code: " ++ show code

	 
