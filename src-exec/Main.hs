module Main(main) where

import           System.Environment         (getArgs)
import           Control.Monad              (when)
import           System.Console.GetOpt      (ArgDescr (NoArg, ReqArg),
                                             ArgOrder (Permute), OptDescr (..),
                                             getOpt, usageInfo)
import           System.Exit                (ExitCode (ExitFailure), exitWith)
import           System.IO                  (hPutStrLn, stderr)                                             
import           XmlToJson

main :: IO ()
main = do
  args <- getArgs
  (flags, inputFiles) <- parseOptions args

  when (elem ShowHelp flags || (null flags && null inputFiles)) .
    die $ usageInfo usageHeader options

  xmlToJson flags inputFiles

options :: [OptDescr Flag]
options =
  [ Option "h" ["help"]             (NoArg ShowHelp)         "Show this help"
  , Option "t" ["tag-name"]         (ReqArg StartFrom "TAG") "Start conversion with nodes named TAG (ignoring all parent nodes)"
  , Option "s" ["skip-roots"]       (NoArg SkipRoots)       ("Ignore the selected nodes, and start converting from their children\n"
                                                             ++ "(can be combined with the 'start-tag' option to process only children of the matching nodes)")
  , Option "a" ["as-array"]         (NoArg WrapArray)        "Output the resulting objects in a top-level JSON array"
  , Option "m" ["multiline"]        (NoArg Multiline)       ("When using 'as-array' output, print each of top-level json object on a seperate line.\n" 
                                                             ++ "(If not using 'as-array', this option will be on regardless, and output is always line-seperated.)")
  , Option ""  ["no-collapse-text"] (ReqArg NoCollapseText "PATTERN") ("For elements with tag matching regex PATTERN only (use '.*' or '' to match all elements):\n"
                                                                       ++ "Don't collapse elements that only contain text into a simple string property.\n"
                                                                       ++ "Instead, always emit '.value' properties for text nodes, even if an element contains only text.\n"
                                                                       ++ "(Output 'schema' will be more stable.)")
  , Option ""  ["no-ignore-nulls"]  (NoArg NoIgnoreNulls)    "Don't ignore nulls (and do output them) in the top level of output objects"
  ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

usageHeader :: String
usageHeader = "Usage: <program> [OPTION...] files..."

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)

  
  
