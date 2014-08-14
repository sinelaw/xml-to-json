module Text.XML.JSON.StreamingXmlToJson
where

import Text.HTML.TagSoup
import qualified Data.Text as T
import qualified Data.Foldable


xmlStreamToJSON :: T.Text -> [T.Text]
xmlStreamToJSON fileData = map toText linesWithLevels
    where xmlData = parseTags fileData
          jsonData = map getEncodedJSON . parseXML $ xmlData
          linesWithLevels = Data.Foldable.toList jsonData

parseXML :: [Tag T.Text] -> [State]
parseXML d = scanl convertTag (State Empty []) d

type Attrs = [(T.Text, T.Text)]
type Name = T.Text
type Elem = (Name, Attrs, Int)
data State = State { getEncodedJSON :: EncodedJSON, getParents :: [Int] }

data EncodedJSON = StartObject Name Attrs Bool | EndObject | Text T.Text Bool | Empty

quoteT :: T.Text
quoteT = T.pack "\""

toText :: EncodedJSON -> T.Text
toText Empty = T.empty
toText (Text t hasLeadingComma) = T.concat [T.pack leadingComma, quoteT, encodeStr $ t, quoteT]
    where leadingComma = if hasLeadingComma then ", " else ""
toText EndObject = T.pack "]}\n"
toText (StartObject name attrs hasLeadingComma) = T.concat [ T.pack (leadingComma ++ "{\"name\": \"")
                                                           , name
                                                           , T.pack "\", "
                                                           , toTextAttrs attrs
                                                           , T.pack "\"items\": [ "
                                                           ]
    where leadingComma = if hasLeadingComma then ", " else ""

toTextAttrs :: Attrs -> T.Text
toTextAttrs [] = T.empty
toTextAttrs as = T.concat [ T.pack "\"attrs\": { "
                          , T.intercalate (T.pack ", ") . map toTextKV $ as
                          , T.pack " }, "
                          ]

toTextKV :: (T.Text, T.Text) -> T.Text
toTextKV (k,v) = T.concat [quoteT, k, T.pack "\": \"", v, quoteT]

-- TODO: use a faster method for quotation escaping. Consider implementing the encoding function using T.Text (or ByteString)
encodeStr :: T.Text -> T.Text
encodeStr t = T.replace (T.pack "\"") (T.pack "\\\"") t

convertTag :: State -> Tag T.Text -> State
convertTag (State _ (curCount:parents)) (TagOpen name attrs) 
    = State startObj (0 : (curCount + 1) : parents)
      where startObj = createStartObject name attrs (curCount > 0)
convertTag (State _ []) (TagOpen name attrs) 
    = State startObj [0]
      where startObj = createStartObject name attrs False
convertTag (State _ ( _:ancestors)) (TagClose _)
    = State EndObject ancestors
convertTag (State _ []) t@(TagClose _)
    = error $ "Malformed XML, unexpected close tag: " ++ (show t)
convertTag (State _ parents) (TagText text) 
    = if stripped == T.empty 
      then State Empty parents
      else State (Text stripped comma) newParents
      where stripped = T.strip text
            (comma, newParents) = case parents of
                      [] -> (False, [])
                      count:ps -> (count > 0, (count + 1):ps)
convertTag (State _ parents) _ = (State Empty parents)

createStartObject :: T.Text -> Attrs -> Bool -> EncodedJSON
createStartObject name attrs hasLeadingComma 
    = case T.head name of
        '!' -> Empty
        '?' -> Empty
        _ -> StartObject name attrs hasLeadingComma

