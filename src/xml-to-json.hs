module Main
where
 
import Text.XML.HXT.Core
import Text.XML.HXT.Expat -- TagSoup
import Text.XML.HXT.Curl -- use libcurl for HTTP access
                         -- only necessary when reading http://...
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable, hash)
import Data.Tree.NTree.TypeDefs 
import System.Environment
import qualified Data.Vector as Vector
import Data.Maybe (isJust, fromJust)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS

--instance XmlPickler Aeson.Value where
--  xpickle = xpJSON
    
--xpJSON :: PU Aeson.Value
--xpJSON = xpElem "
  
main :: IO ()
main = do
       [src] <- getArgs
       [rootElem] <- runX ( readDocument [withValidate no
                                          --,withTagsSoup
                                         ,withExpat True
                                         ,withCurl []
                                         ] src
                            >>> getChildren 
                            >>> isElem
                          )
       --print rootElem
       BS.putStr . Aeson.encode . wrapRoot $ xmlTreeToJSON rootElem
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
                


  