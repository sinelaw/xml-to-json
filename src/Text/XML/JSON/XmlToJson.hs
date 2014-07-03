module Text.XML.JSON.XmlToJson(xmlToJson, Flag(..)) where

import           Control.Applicative        ((*>), (<*))
import           Control.Arrow              (first, (&&&), (***), (>>>))
import           Control.Arrow.ArrowTree    (ArrowTree)
import           Control.Category           (id)
import           Control.Monad              (forM_)
import           Data.Maybe                 (catMaybes)
import           Data.Tree.NTree.TypeDefs
import           Data.Tree.Class			(Tree)
import           Prelude                    hiding (id)
import           Text.Regex.Posix           ((=~))
import           Text.XML.HXT.Core          (ArrowXml, XNode (..), XmlTree,
                                             deep, getAttrl, getChildren,
                                             getName, getText, hasName, isElem,
                                             localPart, no, readDocument, runLA,
                                             runX, withValidate)

#ifdef UseCurl	  
import Text.XML.HXT.Curl -- use libcurl for HTTP access, only necessary when reading http://...
#endif

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Vector                as Vector
import           Text.XML.HXT.Expat         (withExpat)


data Flag = Input String | StartFrom String | Multiline | SkipRoots | NoIgnoreNulls | WrapArray | NoCollapseText String | ShowHelp
    deriving (Show, Eq)

getStartNodes :: ArrowXml cat => [Flag] -> cat (NTree XNode) XmlTree
getStartNodes flags =
  case [x | StartFrom x <- flags] of
  []  -> getChildren >>> isElem
  [x] -> deep (isElem >>> hasName x)
  _   -> error "Expecting at most one --tag-name (-t) option"

getCollapseTextRegex :: [Flag] -> Maybe String
getCollapseTextRegex flags = singleOrNothing "Expecting at most one --no-collapse-text option" [x | NoCollapseText x <- flags]

wrapAction :: [Flag] -> IO a -> IO a
wrapAction flags act
  | WrapArray `elem` flags = putStr "[" *> act <* putStr "]"
  | otherwise = act

multiline :: [Flag] -> [BS.ByteString] -> BS.ByteString
multiline flags = case (WrapArray `elem` flags, Multiline `elem` flags) of
  (False, _)     -> BS.intercalate (BS.pack "\n")
  (True,  False) -> BS.intercalate (BS.pack ",")
  (True,  True)  -> BS.intercalate (BS.pack ",\n")

ignoreNulls :: [Flag] -> [Aeson.Value] -> [Aeson.Value]
ignoreNulls flags
  | NoIgnoreNulls `notElem` flags =
    filter (/= Aeson.Null)
  | otherwise =  id

nodesFilter :: (Data.Tree.Class.Tree t, Control.Arrow.ArrowTree.ArrowTree a) => [Flag] -> a (t b) (t b)
nodesFilter flags
  |  SkipRoots `elem` flags = getChildren
  | otherwise = id

xmlToJson :: [Flag] -> [String] -> IO ()
xmlToJson flags inputFiles =
  forM_ inputFiles $ \src -> do
    rootElems <-
      runX $
      readDocument
      [ withValidate no
      , withExpat True
#ifdef UseCurl	  
      , withCurl []
#endif	  
      ]
      src
      >>> getStartNodes flags
      >>> nodesFilter flags
    -- TODO: de-uglify and optimize the following
    wrapAction flags
      . BS.putStr . multiline flags
      . map Aeson.encode
      . ignoreNulls flags
      . map (wrapRoot . xmlTreeToJSON (getCollapseTextRegex flags))
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
tagMapToJSValue :: Bool -> M.Map JSValueName Aeson.Value -> Aeson.Value
tagMapToJSValue collapseTextRegex m = case (collapseTextRegex, M.toList m) of
  (True, [(Text, val)]) -> val
  _                     ->
    Aeson.Object . HashMap.fromList . (map . first) packJSValueName $ M.toList m

xmlTreeToJSON :: Maybe String -> XmlTree -> Maybe (JSValueName, Aeson.Value)
xmlTreeToJSON collapseTextRegex node@(NTree (XTag qName _) children)
  = Just (Tag (localPart qName),
          tagMapToJSValue shouldCollapseText objMap)
  where
    objMap =
        arrayValuesToJSONArrays    -- unify into a single map,
      . concatMapValues            -- grouping into arrays by pair name
      . map (uncurry M.singleton)  -- convert pairs to maps
      . (++) attrVals
      . catMaybes                  -- filter out the empty values (unconvertable nodes)
      $ map (xmlTreeToJSON collapseTextRegex) children -- convert xml nodes to Maybe (QName, Aeson.Value) pairs

    attrVals =
      map (Attr *** Aeson.String . T.pack) $ getAttrVals node

    shouldCollapseText = case collapseTextRegex of
                         Nothing -> True
                         Just "" -> False
                         Just pattern -> not $ localPart qName =~ pattern

xmlTreeToJSON _ (NTree (XText str) _)
  | T.null text = Nothing
  | otherwise = Just (Text, Aeson.String text)
  where
    text = T.strip $ T.pack str

xmlTreeToJSON _ _ = Nothing

singleOrNothing :: String -> [a] -> Maybe a
singleOrNothing _   []  = Nothing
singleOrNothing _   [x] = Just x
singleOrNothing msg _   = error msg


