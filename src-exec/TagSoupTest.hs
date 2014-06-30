{-# LANGUAGE BangPatterns #-} 
module Main  where

import Text.HTML.TagSoup
import Control.Exception
import Control.Monad
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Environment (getArgs)
import qualified Data.Text
import qualified Data.Foldable


strip :: String -> String
strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack



openItem :: String -> IO String
openItem url | not $ "http://" `isPrefixOf` url = readFile url
openItem url = bracket
    (openTempFile "." "tagsoup.tmp")
    (\(file,_) -> removeFile file)
    $ \(file,hndl) -> do
        hClose hndl
        putStrLn $ "Downloading: " ++ url
        res <- system $ "wget " ++ url ++ " -O " ++ file
        when (res /= ExitSuccess) $ error $ "Failed to download using wget: " ++ url
        src <- readFile file
        length src `seq` return src

main :: IO ()
main = do
    args <- getArgs
    fileData <- openItem . head $ args
    let xmlData = parseTags fileData
    let jsonData = map getEncodedJSON . parseXML $ xmlData
    let linesWithLevels = Data.Foldable.toList jsonData
    let jsonLines = map show linesWithLevels
    forM_ jsonLines putStrLn

parseXML :: [Tag String] -> [State]
parseXML d = scanl convertTag (State Empty []) d

type Attrs = [(String, String)]
type Name = String
type Elem = (Name, Attrs, Int)
data State = State { getEncodedJSON :: EncodedJSON, getParents :: [Int] }
           deriving (Show)

data EncodedJSON = StartObject Name Attrs Bool | EndObject | Text String Bool | Empty

instance Show EncodedJSON where
    show Empty = ""
    show (Text t hasLeadingComma) = leadingComma ++ "\"" ++ encodeStr t ++ "\""
        where leadingComma = if hasLeadingComma then ", " else ""
    show EndObject = "]}\n"
    show (StartObject name attrs hasLeadingComma) = leadingComma 
                                                    ++ "{\"name\": \"" ++ name ++ "\", " 
                                                    ++ showAttrs attrs
                                                    ++ "\"items\": [ "
        where leadingComma = if hasLeadingComma then ", " else ""
              showAttrs [] = ""
              showAttrs as = "\"attrs\": { " ++ (intercalate ", " . map showKV $ as) ++ " }, "
              showKV (k,v) = "\"" ++ k ++ "\": \"" ++ v ++ "\""

-- TODO: use a faster method for quotation escaping. Consider implementing the encoding function using Text (or ByteString)
encodeStr :: String -> String
encodeStr [] = ""
encodeStr (x:xs) = case x of
                     '"' -> ['\\', '"'] ++ (encodeStr xs)
                     _ -> (x : encodeStr xs)

convertTag :: State -> Tag String -> State
convertTag (State _ (curCount:parents)) (TagOpen name attrs) 
    = State startObj (0 : (curCount + 1) : parents)
      where startObj = createStartObject name attrs (curCount > 0)
convertTag (State _ []) (TagOpen name attrs) 
    = State startObj [0]
      where startObj = createStartObject name attrs False
convertTag (State _ ( _:ancestors)) (TagClose _)
    = State EndObject ancestors
      --else error $ "Malformed XML, unexpected close tag: " ++ (show t) ++ " when expecting to close: " ++ expectedName
convertTag (State _ []) t@(TagClose _)
    = error $ "Malformed XML, unexpected close tag: " ++ (show t)
convertTag (State _ parents) (TagText text) 
    = case stripped of
        "" -> State Empty parents
        _ -> State (Text stripped comma) newParents
      where stripped = strip text
            (comma, newParents) = case parents of
                      [] -> (False, [])
                      count:ps -> (count > 0, (count + 1):ps)
convertTag (State _ parents) _ = (State Empty parents)

createStartObject :: [Char] -> Attrs -> Bool -> EncodedJSON
createStartObject name attrs hasLeadingComma 
    = case head name of
        '!' -> Empty
        '?' -> Empty
        _ -> StartObject name attrs hasLeadingComma
