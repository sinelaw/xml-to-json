module Main
where
  
import System.IO  
import System.Exit  
import System.Console.GetOpt  
import Text.XML.HXT.Core ((>>>), (&&&), readDocument, getChildren, getText, isElem, 
                          XmlTree, XNode(..), deep, getName, localPart, hasName, 
                          ArrowXml, runLA, getAttrl, runX, withValidate, no, yes)
import Text.XML.HXT.Expat
import Text.XML.HXT.Curl -- use libcurl for HTTP access
                         -- only necessary when reading http://...
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable, hash)
import Data.Tree.NTree.TypeDefs 
import System.Environment
import qualified Data.Vector as Vector
import Data.Maybe (isJust, fromJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad (when)
  

data Flag = Input String | StartFrom String | Multiline | SkipRoots | IgnoreNulls | WrapArray | ShowHelp
    deriving (Show, Eq)

inp :: Maybe String -> Flag
inp  = Input  . fromMaybe "stdin"

options :: [OptDescr Flag]
options =
     [ Option ['h']     ["help"] (NoArg ShowHelp) "Show this help" 
     , Option ['i']     [] (OptArg inp "FILE")       "input FILE"
     , Option ['t']     ["tag-name"]  (ReqArg StartFrom "TAG") "Start conversion with nodes named TAG (ignoring all parent nodes)"
     , Option ['s']     ["skip-roots"] (NoArg SkipRoots) "Ignore the selected nodes, and start converting from their children (can be combined with the 'start-tag' option to process only children of the matching nodes)"
     , Option ['m']     ["multiline"]  (NoArg Multiline) "Output each of the top-level converted json objects on a seperate line"
     , Option ['n']     ["ignore-nulls"] (NoArg IgnoreNulls) "Ignore nulls (do not output them) in the top-level output objects"
     , Option ['a']     ["as-array"] (NoArg WrapArray) "Output the resulting objects in a top-level JSON array"
     ]

parseOptions :: [String] -> IO ([Flag], [String])
parseOptions argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usageHeader options))
                  
usageHeader :: String
usageHeader = "Usage: <program> [OPTION...] files..."
            
getStartNodes :: ArrowXml cat => [Flag] -> cat (NTree XNode) XmlTree
getStartNodes flags = case (filter f' flags) of
  []            -> getChildren >>> isElem
  [StartFrom x] -> deep (isElem >>> hasName x) 
  _   -> error "Expecting at most one 'start-tag' option"
  where f' (StartFrom _) = True
        f' _             = False
        
getSrcFile :: [Flag] -> String
getSrcFile flags = case (filter f' flags) of
  [Input x] -> x
  _   -> error "Expecting an input file"
  where f' (Input _) = True
        f' _         = False
 
main :: IO ()
main = do
       args <- getArgs
       (flags, _) <- parseOptions args
       
       case (elem ShowHelp flags) of 
         True  -> die $ usageInfo usageHeader options
         False -> return ()
       
       let startNodesFilter = getStartNodes flags
           src = getSrcFile flags
           wrapArray = elem WrapArray flags
           skipRoots = elem SkipRoots flags
           multiline = case (elem Multiline flags, wrapArray) of
             (False, False) -> BS.concat
             (False, True)  -> BS.intercalate (BS.pack ",") 
             (True,  False) -> BS.intercalate (BS.pack "\n") 
             (True,  True)  -> BS.intercalate (BS.pack ",\n") 
             
           ignoreNulls = case (elem IgnoreNulls flags) of
             False  -> const True
             True   -> (/= Aeson.Null)
             
           nodesFilter = case skipRoots of
             False -> startNodesFilter
             True  -> startNodesFilter >>> getChildren
             
       rootElems <- runX ( readDocument [withValidate no
                                        ,withExpat True
                                        ,withCurl []
                                        ] src
                           >>> nodesFilter )

       -- TODO: de-uglify and optimize the following
       when wrapArray $ putStr "["
       BS.putStr $ multiline
                 . map Aeson.encode 
                 . filter ignoreNulls 
                 . map (wrapRoot . xmlTreeToJSON)
                 $ rootElems
       when wrapArray $ putStr "]"
       
       return ()
      
data JSValueName = Text | Tag String | Attr String
     deriving (Eq, Ord, Show)

instance Hashable JSValueName where
  hash Text     = 0
  hash (Tag s)  = hash s
  hash (Attr s) = hash s
  
concatMapValues :: (Eq k, Hashable k) => [M.HashMap k v] -> M.HashMap k [v] 
concatMapValues = foldr (M.unionWith (++) . M.map (:[])) M.empty 

fromObject :: Aeson.Value -> Maybe Aeson.Object
fromObject (Aeson.Object objMap) = Just objMap
fromObject _                     = Nothing

getAttrVals :: XmlTree -> [(String, String)]
getAttrVals node = runLA (getAttrl >>> getName &&& (getChildren >>> getText)) node

arrayValuesToJSONArrays :: (Eq k, Hashable k) => M.HashMap k [Aeson.Value] -> M.HashMap k Aeson.Value
arrayValuesToJSONArrays = M.map fromJust
                        . M.filter isJust
                        . M.map (\v -> case v of 
                                    []  -> Nothing -- will be discarded
                                    [x] -> Just x  -- don't store as array, just a single value
                                    xss -> Just $ Aeson.Array . Vector.fromList $ xss) -- arrays with more than one element are kept

-- apply on object internal maps, ignore non-object json values (don't apply on arrays)
fmapObj :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
fmapObj f (Aeson.Object objMap) = Aeson.Object (f objMap)
fmapObj _ val                   = val 

packJSValueName :: JSValueName -> T.Text 
packJSValueName Text = T.pack "value"
packJSValueName (Attr x) = T.pack x
packJSValueName (Tag x)  = T.pack x

mapKeys :: (Eq k1, Eq k2, Hashable k1, Hashable k2) => (k1 -> k2) -> M.HashMap k1 v -> M.HashMap k2 v
mapKeys f = M.foldrWithKey (\k v -> M.insert (f k) v) M.empty

wrapRoot :: Maybe (JSValueName, Aeson.Value) -> Aeson.Value
wrapRoot Nothing       = Aeson.Null
wrapRoot (Just (a, b)) = Aeson.object [(packJSValueName a, b)] 

-- converts a map to a json value, usually resulting in a json object unless the map contains ONLY a single Text entry, 
-- in which case the value produced is a json string
tagMapToJSValue :: M.HashMap JSValueName Aeson.Value -> Aeson.Value
tagMapToJSValue m = case (M.toList m) of
  [(Text, val)] -> val
  _             -> Aeson.Object . mapKeys packJSValueName $ m
  
xmlTreeToJSON :: XmlTree -> Maybe (JSValueName, Aeson.Value)
xmlTreeToJSON node@(NTree (XTag qName _) children) 
  = Just (Tag (localPart qName), 
          tagMapToJSValue objMap)
  where objMap = arrayValuesToJSONArrays    -- unify into a single map,
               . concatMapValues            -- grouping into arrays by pair name
               . map (uncurry M.singleton)  -- convert pairs to maps
               . (++) attrVals       
               . map fromJust               -- unpack the Just values
               . filter isJust              -- filter out the empty values (unconvertable nodes)
               $ map xmlTreeToJSON children -- convert xml nodes to Maybe (QName, Aeson.Value) pairs
        
        attrVals = map (\(name, val) -> (Attr name, Aeson.String (T.pack val))) $ getAttrVals node        

xmlTreeToJSON (NTree (XText str) _) = if (T.empty == text) 
                                      then Nothing
                                      else Just $ (Text, Aeson.String text)
  where text = T.strip . T.pack $ str

xmlTreeToJSON _ = Nothing 
                


die :: String -> IO a
die msg = do putErrLn (msg)
             exitWith (ExitFailure 1)
             

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr
