module Text.XML.JSON.StreamingXmlToJson(xmlStreamToJSON)
where

import Text.HTML.TagSoup
import qualified Data.Text.Lazy as T
import Data.List (intercalate)
import qualified Data.Foldable



xmlStreamToJSON :: String -> [String]
xmlStreamToJSON fileData = map toText linesWithLevels
    where xmlData = parseTags fileData
          jsonData = map getEncodedJSON . parseXML $ xmlData
          linesWithLevels = Data.Foldable.toList jsonData

parseXML :: [Tag String] -> [State]
parseXML d = scanl convertTag (State Empty []) d

type Attrs = [(String, String)]
type Name = String
data State = State { getEncodedJSON :: EncodedJSON, getParents :: [Int] }

data EncodedJSON = StartObject Name Attrs Bool | EndObject | Text String Bool | Empty

quoteT :: String
quoteT = "\""

toText :: EncodedJSON -> String
toText Empty = ""
toText (Text t hasLeadingComma) = concat [leadingComma, quoteT, encodeStr t, quoteT]
    where leadingComma = if hasLeadingComma then ", " else ""
toText EndObject = "]}\n"
toText (StartObject name attrs hasLeadingComma) = concat [ leadingComma
                                                         , "{\"name\": \""
                                                         , name
                                                         , "\", "
                                                         , toTextAttrs attrs
                                                         , "\"items\": [ "
                                                         ]
    where leadingComma = if hasLeadingComma then ", " else ""

toTextAttrs :: Attrs -> String
toTextAttrs [] = ""
toTextAttrs as = concat [ "\"attrs\": { "
                          , intercalate (", ") . map toTextKV $ as
                          , " }, "
                          ]

toTextKV :: (String, String) -> String
toTextKV (k,v) = concat [quoteT, k, "\": \"", v, quoteT]

-- TODO: use a faster method for quotation escaping. Consider implementing the encoding function using String (or ByteString)
encodeStr :: String -> String
encodeStr t = concat . map (\c -> if c == '"' 
                                 then "\\\"" 
                                 else [c]) 
              $ t

convertTag :: State -> Tag String -> State
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
    = if stripped == ""
      then State Empty parents
      else State (Text stripped comma) newParents
      where stripped = T.unpack . T.strip . T.pack $ text
            (comma, newParents) = case parents of
                      [] -> (False, [])
                      count:ps -> (count > 0, (count + 1):ps)
convertTag (State _ parents) _ = (State Empty parents)

createStartObject :: String -> Attrs -> Bool -> EncodedJSON
createStartObject name attrs hasLeadingComma 
    = case head name of
        '!' -> Empty
        '?' -> Empty
        _ -> StartObject name attrs hasLeadingComma

