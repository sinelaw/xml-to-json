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
import qualified Data.Aeson.Types as JSON
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector
import qualified Data.Sequence
import Data.Sequence  ((|>))
import qualified Data.Foldable

import Debug.Trace

strip :: String -> String
strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack

traceShowId a = traceShow a a

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
data State = State EncodedJSON [Elem]
           deriving (Show)
getEncodedJSON (State e _) = e

data EncodedJSON = StartObject Name Attrs Bool | EndObject | Text String Bool | Empty

instance Show EncodedJSON where
    show Empty = ""
    show (Text t hasLeadingComma) = leadingComma ++ "'" ++ t ++ "'"
        where leadingComma = if hasLeadingComma then ", " else ""
    show EndObject = "]}\n"
    show (StartObject name attrs hasLeadingComma) = leadingComma 
                                                    ++ "{'name': '" ++ name ++ "', " 
                                                    ++ showAttrs attrs
                                                    ++ "'items': [ "
        where leadingComma = if hasLeadingComma then ", " else ""
              showAttrs [] = ""
              showAttrs as' = "'attrs': { " ++ (intercalate ", " . map showKV $ as') ++ " }, "
              showKV (k,v) = "'" ++ k ++ "': '" ++ v ++ "'"

convertTag :: State -> Tag String -> State
convertTag (State _ ((curName, curAttrs, curCount):parents)) (TagOpen name attrs) 
    = State startObj ((name, attrs, 0) : (curName, curAttrs, curCount + 1) : parents)
      where startObj = StartObject name attrs (curCount > 0)
convertTag (State _ []) (TagOpen name attrs) 
    = State startObj [(name, attrs, 0)]
      where startObj = StartObject name attrs False
convertTag (State _ (parent@(expectedName, _, _):ancestors)) t@(TagClose name)
    = if expectedName == name 
      then State EndObject ancestors
      else error $ "Malformed XML, unexpected close tag: " ++ (show t) ++ " when expecting to close: " ++ expectedName
convertTag (State _ []) t@(TagClose _)
    = error $ "Malformed XML, unexpected close tag: " ++ (show t)
convertTag (State _ parents) t@(TagText text) 
    = case stripped of
        "" -> State Empty parents
        _ -> State (Text stripped comma) parents
      where stripped = strip text
            comma = case parents of
                      [] -> False
                      (_,_,count):_ -> count > 0
convertTag (State _ parents) _ = (State Empty parents)

headl :: Data.Sequence.Seq a -> a
headl s = Data.Sequence.index s 0

taill :: Data.Sequence.Seq a -> Data.Sequence.Seq a
taill s = Data.Sequence.drop 1 s

isObject :: JSON.Value -> Bool
isObject x = case x of 
                  JSON.Object _ -> True
                  _             -> False

buildObject :: JSON.Value -> JSON.Value -> JSON.Value
buildObject attrs items = JSON.object [(Data.Text.pack "attrs", attrs), (Data.Text.pack "items", items)]
