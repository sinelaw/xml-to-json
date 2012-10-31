module Main(main) where

import Control.Category (id)
import Control.Applicative ((<*), (*>))
import Control.Arrow (first, (>>>), (&&&), (***))
import Control.Monad (when, forM_)
import Data.Maybe (catMaybes)
import Data.Tree.NTree.TypeDefs
import Prelude hiding (id)
import System.Console.GetOpt (OptDescr(..), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), usageInfo, getOpt)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import Text.XML.HXT.Core (readDocument, getChildren, getText, isElem, XmlTree, XNode(..), deep, getName, localPart, hasName, ArrowXml, runLA, getAttrl, runX, withValidate, no)
import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
import Text.XML.HXT.Expat (withExpat)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as Vector

data Flag = Input String | StartFrom String | Multiline | SkipRoots | IgnoreNulls | WrapArray | ShowHelp
    deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option "h"     ["help"] (NoArg ShowHelp) "Show this help"
  , Option "t"     ["tag-name"]  (ReqArg StartFrom "TAG") "Start conversion with nodes named TAG (ignoring all parent nodes)"
  , Option "s"     ["skip-roots"] (NoArg SkipRoots) ("Ignore the selected nodes, and start converting from their children\n"
                                                      ++ "(can be combined with the 'start-tag' option to process only children of the matching nodes)")
  , Option "m"     ["multiline"]  (NoArg Multiline) "Output each of the top-level converted json objects on a seperate line"
  , Option "n"     ["ignore-nulls"] (NoArg IgnoreNulls) "Ignore nulls (do not output them) in the top-level output objects"
  , Option "a"     ["as-array"] (NoArg WrapArray) "Output the resulting objects in a top-level JSON array"
  ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))

usageHeader :: String
usageHeader = "Usage: <program> [OPTION...] files..."

getStartNodes :: ArrowXml cat => [Flag] -> cat (NTree XNode) XmlTree
getStartNodes flags =
  case [x | StartFrom x <- flags] of
  []  -> getChildren >>> isElem
  [x] -> deep (isElem >>> hasName x)
  _   -> error "Expecting at most one 'start-tag' option"

main :: IO ()
main = do
  args <- getArgs
  (flags, inputFiles) <- parseOptions args

  when (elem ShowHelp flags || (null flags && null inputFiles)) .
    die $ usageInfo usageHeader options

  let
    wrapArray = WrapArray `elem` flags
    wrapAction act
      | wrapArray = putStr "[" *> act <* putStr "]"
      | otherwise = act
    skipRoots = SkipRoots `elem` flags
    multiline = case (Multiline `elem` flags, wrapArray) of
      (False, False) -> BS.concat
      (False, True)  -> BS.intercalate (BS.pack ",")
      (True,  False) -> BS.intercalate (BS.pack "\n")
      (True,  True)  -> BS.intercalate (BS.pack ",\n")

    ignoreNulls
      | IgnoreNulls `elem` flags =
        filter (/= Aeson.Null)
      | otherwise =  id

    nodesFilter
      | skipRoots = getChildren
      | otherwise = id

  forM_ inputFiles $ \src -> do
    rootElems <-
      runX $
      readDocument
      [ withValidate no
      , withExpat True
      , withCurl []
      ]
      src
      >>> getStartNodes flags
      >>> nodesFilter
    -- TODO: de-uglify and optimize the following
    wrapAction
      . BS.putStr . multiline
      . map Aeson.encode
      . ignoreNulls
      . map (wrapRoot . xmlTreeToJSON)
      $ rootElems

data JSValueName = Text | Tag String | Attr String
  deriving (Eq, Ord, Show)

concatMapValues :: (Ord k) => [M.Map k v] -> M.Map k [v]
concatMapValues = M.unionsWith (++) . (fmap . fmap) (: [])

getAttrVals :: XmlTree -> [(String, String)]
getAttrVals = runLA (getAttrl >>> getName &&& (getChildren >>> getText))

arrayValuesToJSONArrays :: (Ord k) => M.Map k [Aeson.Value] -> M.Map k Aeson.Value
arrayValuesToJSONArrays = M.mapMaybe f
  where
    f [] = Nothing -- will be discarded
    f [x] = Just x  -- don't store as array, just a single value
    f xss = Just $ Aeson.Array . Vector.fromList $ xss -- arrays with more than one element are kept

packJSValueName :: JSValueName -> T.Text
packJSValueName Text = T.pack "value"
packJSValueName (Attr x) = T.pack x
packJSValueName (Tag x)  = T.pack x

wrapRoot :: Maybe (JSValueName, Aeson.Value) -> Aeson.Value
wrapRoot Nothing       = Aeson.Null
wrapRoot (Just (a, b)) = Aeson.object [(packJSValueName a, b)]

-- converts a map to a json value, usually resulting in a json object unless the map contains ONLY a single Text entry,
-- in which case the value produced is a json string
tagMapToJSValue :: M.Map JSValueName Aeson.Value -> Aeson.Value
tagMapToJSValue m = case M.toList m of
  [(Text, val)] -> val
  _             ->
    Aeson.Object . HashMap.fromList . (map . first) packJSValueName $ M.toList m

xmlTreeToJSON :: XmlTree -> Maybe (JSValueName, Aeson.Value)
xmlTreeToJSON node@(NTree (XTag qName _) children)
  = Just (Tag (localPart qName),
          tagMapToJSValue objMap)
  where
    objMap =
        arrayValuesToJSONArrays    -- unify into a single map,
      . concatMapValues            -- grouping into arrays by pair name
      . map (uncurry M.singleton)  -- convert pairs to maps
      . (++) attrVals
      . catMaybes                  -- filter out the empty values (unconvertable nodes)
      $ map xmlTreeToJSON children -- convert xml nodes to Maybe (QName, Aeson.Value) pairs

    attrVals =
      map (Attr *** Aeson.String . T.pack) $ getAttrVals node

xmlTreeToJSON (NTree (XText str) _)
  | T.null text = Nothing
  | otherwise = Just (Text, Aeson.String text)
  where
    text = T.strip $ T.pack str

xmlTreeToJSON _ = Nothing

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitWith (ExitFailure 1)
