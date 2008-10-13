
module Main where


import Data.Metrics
import SGLR (sglr)
import Language.Sdf.SdfLib
import System.Console.GetOpt
import IO (hPutStrLn, hFlush, stderr)
import Control.Monad (when, mapM_, foldM)
import Data.Relation.GraphViz (printGraphWith,printComponentGraphWith)
-- import Data.Relation.Write (printRel)
import Data.Graph.Components
import Data.Relation (transClose) 
import System.Environment (getArgs, getProgName)
import System.Exit (exitWith, exitFailure, ExitCode(..))

import Language.Sdf.FlowGraph
import Language.Sdf.Metrics
import Language.Sdf.Pretty (renderSDF)

import Language.DmsGrammar.DmsLib
import Language.Antlr.AntlrLib as Antlr (Grammar, antlr2sdf)
import Language.Bison.BisonLib (BisonSyntax, bison2sdf)


-- * Data structures

-- | Data type that holds the supported grammar types or formalisms.
data SyntaxFormalism = SdfGrammar | DmsGrammar | AntlrGrammar | BisonGrammar


-- | Record type to hold all program options.
data Opt = Opt
   { -- | VDM source filename that will be used for input
     optInput :: [String]
     -- | Function that will output the final result
   , optOutput :: String -> IO ()
     -- | Function that will print the calculated results from a grammar AST.
   , optPrint :: SyntaxFormalism -> [String] -> IO String
     -- | Type of the grammar that will be used for parsing.
   , optSyntax :: SyntaxFormalism
   }


-- | Default options.
startOpt :: Opt
startOpt = Opt
   { optInput  = []
   , optOutput = putStrLn
   , optPrint  = genMetricsReport
   , optSyntax  = SdfGrammar
   }


-- | Description of all available program options.
options :: [OptDescr (Opt -> IO Opt)]
options =
   [ Option "h" ["help"] 
        (NoArg (\opt -> exitHelp)) 
        "Show usage info"
   , Option "i" ["input"] 
        (ReqArg 
           (\arg opt -> do
              return opt { optInput = arg:(optInput opt) })
           "FILE")        
        "Input file (mandatory)"
   , Option "f" ["syntax-formalism"]
        (ReqArg
           (\arg opt -> do return opt { optSyntax = getSyntaxDefinition arg })
           "Formalism")
           "Grammar formalism used as input (can be one of {\"sdf\", \"dms\", \"antlr\", \"bison\"})"
   , Option "m" ["metrics"]
        (NoArg (\opt -> do 
                  return opt { optPrint = genMetricsReport }))
        "Calculate metrics and present them in nice format"
   , Option "c" ["csv"]
        (NoArg (\opt -> do
                  return opt { optPrint = genMetricsCSV }))
        "Calculate metrics and present them as Comma Separated Values"          
   , Option "g" ["graph"]
        (NoArg (\opt -> do return opt { optPrint = printGph }))
        "Create & print the \"control\" flow graph of the grammar in DOT format"
   , Option "p" ["strong-connected-components"]
        (NoArg (\opt -> do return opt {optPrint = printStrongConnectedComponents}))
        "Create & print the strong connected components of the grammar in DOT format"     
   , Option "n" ["non-singleton"]
        (NoArg (\opt -> do return opt { optPrint = showNonSingletonLevels }))
        "Show non-singleton levels of a grammar" 
   , Option "t" ["transitive-closure"]
        (NoArg (\opt -> do return opt { optPrint = showTransitiveClosure }))
        "Show transitive closure of a grammar"         
   , Option "s" ["sdf"]
        (NoArg (\opt -> do return opt { optPrint = showGrammar }))
        "Show the SDF grammar"
   , Option "o" ["output"]
        (ReqArg
           (\arg opt -> do
              return opt { optOutput = writeFile arg })
           "FILE")
        "Output file (default: stdout)"
   ]



-- * Main function

sdfTable = "../contrib/UMinhoHaskellSoftware/Language/Sdf/Sdf.tbl"
dmsTable = "../contrib/UMinhoHaskellSoftware/Language/DmsGrammar/dms-syntax.def.tbl"
antlrTable = "../contrib/UMinhoHaskellSoftware/Language/Antlr/antlr.tbl"
bisonTable = "../contrib/UMinhoHaskellSoftware/Language/Bison/bison.tbl"


main :: IO ()
main = do 
   opts <- parseOptions
   let Opt { optInput      = input
           , optOutput     = output 
           , optPrint      = print
           , optSyntax     = syntax
           } = opts
   res <- print syntax input
   output res



-- * Functions that support program's functionality

showGrammar :: SyntaxFormalism -> [String] -> IO String
showGrammar sf = processFiles sf renderSDF (unlines . map snd)


-- | Generate string representation of a CSV file from a list of input files.
genMetricsCSV :: SyntaxFormalism -> [String] -> IO String
genMetricsCSV sf = processFiles sf calculateMetrics printMetricsCSV


-- | Generate a metrics report from a list of input files.
genMetricsReport :: SyntaxFormalism -> [String] -> IO String
genMetricsReport sf = processFiles sf calculateMetrics printMetricsReport


-- | Converts an SDF grammar into a graph and print it using the DOT format.
printGph :: SyntaxFormalism -> [String] -> IO String
printGph sf a = do 
   res <- processSingleFile sf createGph (head a)
   return $ snd res
   where createGph = printGraphWith id (const id) "Grammar" . calcGrammarGraph


-- | Converts an SDF grammar into a graph, calculate the strongly connected
--   components and pretty-print it in DOT format.
printStrongConnectedComponents :: SyntaxFormalism -> [String] -> IO String
printStrongConnectedComponents sf a = do
   (_, res) <- processSingleFile sf calcGrammarGraph (head a)
   let (_,scc) = strongComponentGraph res
   --return $ printRel "StronglyConnectedComponents" scc
   return $ printComponentGraphWith show show
                      "StronglyConnectedComponents" scc



-- | Calculate and print the elements of the non-singleton components of the
--   grammar graph.
showNonSingletonLevels :: SyntaxFormalism -> [String] -> IO String
showNonSingletonLevels sf a = do
   res <- processSingleFile sf calcNonSingletonLevels (head a)
   return $ unlines $ map show $ snd res


showTransitiveClosure :: SyntaxFormalism -> [String] -> IO String
showTransitiveClosure sf a = do
   res <- processSingleFile sf createGph (head a)
   return $ snd res
   where createGph = printGraphWith id (const id) "Grammar" . transClose . calcGrammarGraph


-- * Auxiliary functions to print and calculate results


-- | Takes a list of pairs (filename and a list of group metrics) and creates
--   a string representation of a metrics report.
printMetricsReport :: [(String, [GroupMetric])] -> String
printMetricsReport =
   unlines . concatMap (\(f,gm) -> (printFileHeader f) ++ (printGroupMetricList gm))


-- | Creates an header for a metrics report with the filename.
printFileHeader :: String -> [String]
printFileHeader str = [ sep, msg, sep, "" ]
   where msg = "SdfMetz - Metrics report for file: " ++ str
         sep = take (length msg) (repeat '-')      


-- | Takes a list of pairs (filename and a list of group metrics) and creates
--   a string representation of a Comma Separated Values file (CSV).
printMetricsCSV :: [(String, [GroupMetric])] -> String
printMetricsCSV a = unlines $ map (concatWithSep ", ") (header:metrics)
   where header  = "FILE":(printGroupMetricHeaderRow $ snd $ head a)
         metrics = map (\(f,m) -> f:(printGroupMetricRow m)) a


-- | Process multiple input files.
--   Takes as arguments two functions (a conversion and a print function) and
--   a list of files to be processed.
--   The conversion function takes an SDF grammar as input and returns a result
--   of any type.
--   The print function takes a list of pairs (name of the file and the 
--   conversion function results) and creates a string representation from it.
processFiles :: SyntaxFormalism -> (SDF -> a) -> ([(String,a)] -> String) -> [String] -> IO String
processFiles sf convert print files = do
   results <- mapM (processSingleFile sf convert) files
   return $ print results


-- | Processes a single input file. Takes as argument the formalism of the 
--   grammar to be parsed,  a conversion function and a filename.
--   The formalism type allows to choose the correct parser to be used and to 
--   convert the output in an equivalent SDF grammar.
--   The filename will be read and parsed with sglr returning a grammar AST.
--   The result of this function will be a pair with the filename and
--   the conversion function result.
processSingleFile :: SyntaxFormalism -> (SDF -> a) -> String -> IO (String, a)
processSingleFile (SdfGrammar) convert file = do
   sdf <- sglr sdfTable file "SDF" :: IO SDF
   return $ (file, convert sdf)
processSingleFile (DmsGrammar) convert file = do
   dms <- sglr dmsTable file "DmsSyntax" :: IO DmsSyntax
   return $ (file, (convert . dmsGrammar2Sdf) dms)
processSingleFile (AntlrGrammar) convert file = do
   antlr <- sglr antlrTable file "Grammar" :: IO Antlr.Grammar
   return $ (file, (convert . antlr2sdf) antlr)
processSingleFile (BisonGrammar) convert file = do
   bison <- sglr bisonTable file "BisonSyntax" :: IO BisonSyntax
   return $ (file, (convert . bison2sdf) bison)


-- | Converts the string representation of the grammar formalism to be used to
--   its equivalent data type.
getSyntaxDefinition :: String -> SyntaxFormalism
getSyntaxDefinition "sdf" = SdfGrammar
getSyntaxDefinition "dms" = DmsGrammar
getSyntaxDefinition "antlr" = AntlrGrammar
getSyntaxDefinition "bison" = BisonGrammar
getSyntaxDefinition str   = error $ "Syntax definition format unknown " ++ str


-- | Concatenate strings with separator.
concatWithSep :: String -> [String] -> String
concatWithSep a []    = ""
concatWithSep a [s]   = s
concatWithSep a (h:t) = h ++ a ++ concatWithSep a t



-- * Functions to parse and deal with program options

-- | Function that reads arguments from the environment and parses it according
-- with the rules specified in the 'options' type.
parseOptions :: IO Opt
parseOptions = do
   -- Reads the options to a triple
   (optsActions, rest, errs) <- getArgs >>= return . getOpt RequireOrder options

   -- If there are errors show them, show the usage and exit with failure code
   when (not (null errs)) $ do
      mapM_ (hPutStrLn stderr) errs
      showHelp
      exitFailure
   
   -- If no options were specified then show the help and exit with failure 
   -- code. The "rest" is used to save multiple files passed as options, thus
   -- it must be tested.
   when ((null optsActions) && (null rest)) $ do
      showHelp
      exitFailure
            
   -- create a record with all program options            
   opts <- foldl (>>=) (return startOpt) optsActions
   
   -- verify if at least one input argument exists. This can be specified both
   -- with "i" option or just passing the names of the files 
   when ((null rest) && (optInput opts == [])) $ do
      exitErrorHelp "At least one input file is required"
      
   -- return all the parsed options   
   return opts { optInput = (optInput opts) ++ rest }


-- | Function that prints the help usage and returns with a success code (used
--   when the help program option is specified.
exitHelp :: IO a
exitHelp = do
    showHelp
    exitWith ExitSuccess


-- | Function that prints the program usage to the sdtderr using the standard
--   'usageInfo' function.
showHelp :: IO ()
showHelp = do
    prg <- getProgName
    hPutStrLn stderr (usageInfo prg options)
    hFlush stderr


-- | Helper function that prints a message to stderr and exits with a failure
--   code error
exitErrorHelp :: String -> IO ()
exitErrorHelp msg = do
   hPutStrLn stderr ("Error: " ++ msg)
   hPutStrLn stderr ""
   showHelp
   exitFailure

