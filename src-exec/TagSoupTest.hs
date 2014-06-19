module Main  where

import Text.HTML.TagSoup

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import           System.Environment         (getArgs)
import qualified Data.Text
import Data.Aeson.Types
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector
import Debug.Trace

strip :: String -> String
strip = Data.Text.unpack . Data.Text.strip . Data.Text.pack

--traceShowId a = traceShow a a


openItem :: String -> IO String
openItem url | not $ "http://" `isPrefixOf` url = readFile url
openItem url = bracket
    (openTempFile "." "tagsoup.tmp")
    (\(file,hndl) -> removeFile file)
    $ \(file,hndl) -> do
        hClose hndl
        putStrLn $ "Downloading: " ++ url
        res <- system $ "wget " ++ url ++ " -O " ++ file
        when (res /= ExitSuccess) $ error $ "Failed to download using wget: " ++ url
        src <- readFile file
        length src `seq` return src

main = do
    args <- getArgs
    fileData <- openItem . head $ args
    let linesWithLevels = foldl  (flip convertTag) [] . parseTags $ fileData
    --let lines = map (\(level, text) -> (take (4*level) $ repeat ' ') ++ text ++ "\n") $ linesWithLevels
    let lines = map (encode . snd) linesWithLevels
    forM_ lines BS.putStrLn

traceJobj jobj = trace ("hi:" ++ (show . encode $ jobj)) jobj
trace' x = traceStack ("yo:" ++ (show x)) x
convertTag' a b = traceJobj (convertTag a b)

convertTag :: Tag String -> [(Bool, Data.Aeson.Types.Value)] -> [(Bool, Data.Aeson.Types.Value)]
convertTag (TagOpen name attrs) xs = (True, object (map toPair attrs)) : xs
    where toPair (k,v) = (Data.Text.pack k, String (Data.Text.pack v))
convertTag (TagClose name) xs = (False, object [(Data.Text.pack name, innerObj)]) : tail parents
    where (children, parents) = break fst xs
          items = Array (Data.Vector.fromList . map snd $ children)
          attrs = snd . head $ parents
          innerObj = buildObject attrs items

convertTag (TagText text) xs = case s of
                                   "" -> xs
                                   _  -> (False, String (Data.Text.pack s)) : xs
                               where s = strip text
convertTag _ xs = xs

isObject x = case x of 
                 Object _ -> True
                 _        -> False

buildObject attrs items = object [(Data.Text.pack "attrs", attrs), (Data.Text.pack "items", items)]