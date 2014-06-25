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
import Data.Sequence  ((<|))
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
    let (State jsonData _) = parseXML xmlData
    let linesWithLevels = Data.Foldable.toList jsonData
    let jsonLines = map show linesWithLevels
    forM_ jsonLines putStrLn

    --forM_ xmlData (putStrLn . show)
    -- let folded = foldr (\t x -> show t : x) [] xmlData
    -- forM_ folded putStrLn

-- traceJobj jobj = trace ("hi:" ++ (show . encode $ jobj)) jobj
-- trace' x = traceStack ("yo:" ++ (show x)) x
-- convertTag' a b = traceJobj (convertTag a b)

--parseXML :: [Tag String] -> [(Bool, JSON.Value)]
parseXML d = foldl' convertTag (State [] []) $ d

type Attrs = [(String, String)]
type Name = String
type Elem = (Name, Attrs, Int)
-- type EncodedElems = Data.Sequence.Seq EncodedJSON
-- type Elems = Data.Sequence.Seq Elem
data State = State [EncodedJSON] [Elem] 
           deriving (Show)

data EncodedJSON = StartObject Name Attrs | EndObject | Comma
                 deriving (Show)

convertTag :: State -> Tag String -> State
convertTag (State encs ((curName, curAttrs, curCount):parents)) (TagOpen name attrs) 
    = State encs' ((name, attrs, 0) : (curName, curAttrs, curCount + 1) : parents)
      where encs' = case curCount of
                      0 -> startObj : encs
                      _ -> startObj : Comma : encs
            startObj = StartObject name attrs
convertTag (State encs []) (TagOpen name attrs) 
    = State (startObj : encs) [(name, attrs, 0)]
      where startObj = StartObject name attrs
convertTag (State encs (parent@(expectedName, _, _):ancestors)) t@(TagClose name)
    = if expectedName == name 
      then State (EndObject : encs) ancestors
      else error $ "Malformed XML, unexpected close tag: " ++ (show t) ++ " when expecting to close: " ++ expectedName
convertTag (State encs []) t@(TagClose _)
    = error $ "Malformed XML, unexpected close tag: " ++ (show t)
convertTag state _ = state

-- showOpen :: Name -> Attrs -> String
-- showOpen name attrs = "{ \"" ++ name ++ "\": \n" ++ showAttrs
-- where showAttrs attrs = " { \"attrs\": { " ++ (join ", " $ map (\x -> "\"" ++ k ++ "\": \"" ++ v ++ "\"") attrs) ++ " }\n"

-- data Object a = Object a [(a,a)] [Object a]

-- instance (Show a) => Show (Object a) where
--     show (Object name attrs items) = "{ " ++ (show name) ++ ": [" ++ (show . length $ items) ++ "] }"

-- convertTag ::  [(Bool, Object String)] -> Tag String -> [(Bool, Object String)]
-- convertTag xs (TagOpen name attrs) = traceShowId $ (True, Object name attrs []) : xs
-- --    where toPair (k, v) = (Data.Text.pack k, JSON.String (Data.Text.pack v))
-- convertTag xs (TagClose name) = traceShowId $ (False, Object name attrs items) : (tail parents)
--     where (children, parents) = break fst xs
--           items = map snd children
--           (Object _ attrs _) = snd . head $ parents
-- convertTag xs _ = xs



-- convertTag :: Data.Sequence.Seq (Bool, JSON.Value) -> Tag String -> Data.Sequence.Seq (Bool, JSON.Value)
-- convertTag xs (TagOpen _ attrs) = {-# SCC convertTag_Open #-} (True, JSON.object (map toPair attrs)) <| xs
--     where toPair (k, v) = (Data.Text.pack k, JSON.String (Data.Text.pack v))
-- convertTag xs (TagClose name) = {-# SCC convertTag_Close #-} (False, JSON.object [(Data.Text.pack name, innerObj)]) <| taill parents
--     where (children, parents) = Data.Sequence.breakl fst xs
--           items = JSON.Array (Data.Vector.fromList . map snd . Data.Foldable.toList $ children)
--           attrs = snd . headl $ parents
--           innerObj = buildObject attrs items
-- convertTag xs (TagText text) = {-# SCC convertTag_Text #-} case s of
--                                     "" -> xs
--                                     _  -> (False, JSON.String (Data.Text.pack s)) <| xs
--                                 where s = strip text
-- convertTag xs _ = {-# SCC convertTag_Other #-} xs


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
