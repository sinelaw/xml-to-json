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

strip :: String -> String
strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack

--traceShowId a = traceShow a a

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
    let linesWithLevels = Data.Foldable.toList . parseXML $ fileData
    --let lines = map (\(level, text) -> (take (4*level) $ repeat ' ') ++ text ++ "\n") $ linesWithLevels
    let jsonLines = map (encode . snd) linesWithLevels
    forM_ (reverse jsonLines) BS.putStrLn

-- traceJobj jobj = trace ("hi:" ++ (show . encode $ jobj)) jobj
-- trace' x = traceStack ("yo:" ++ (show x)) x
-- convertTag' a b = traceJobj (convertTag a b)

parseXML :: String -> Data.Sequence.Seq (Bool, JSON.Value)
parseXML d = foldl (flip (\x -> convertTag $! x)) Data.Sequence.empty . parseTags $ d

convertTag :: Tag String -> Data.Sequence.Seq (Bool, JSON.Value) -> Data.Sequence.Seq (Bool, JSON.Value)
convertTag (TagOpen _ attrs) xs = (True, JSON.object (map toPair attrs)) <| xs
    where toPair (k,v) = (Data.Text.pack k, JSON.String (Data.Text.pack v))
convertTag (TagClose name) xs = (False, JSON.object [(Data.Text.pack name, innerObj)]) <| taill parents
    where (children, parents) = Data.Sequence.breakl fst xs
          items = JSON.Array (Data.Vector.fromList . map snd . Data.Foldable.toList $! children)
          attrs = snd . headl $! parents
          innerObj = buildObject attrs items

convertTag (TagText text) xs = case s of
                                   "" -> xs
                                   _  -> (False, JSON.String (Data.Text.pack s)) <| xs
                               where s = strip text
convertTag _ xs = xs


headl :: Data.Sequence.Seq a -> a
headl s = Data.Sequence.index s 0

taill :: Data.Sequence.Seq a -> Data.Sequence.Seq a
taill s = Data.Sequence.drop 1 s

isObject :: JSON.Value -> Bool
isObject x = case x of 
                 JSON.Object _ -> True
                 _        -> False

buildObject :: JSON.Value -> JSON.Value -> JSON.Value
buildObject attrs items = JSON.object [(Data.Text.pack "attrs", attrs), (Data.Text.pack "items", items)]
