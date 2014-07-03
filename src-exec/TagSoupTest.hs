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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Foldable
import Text.XML.JSON.StreamingXmlToJson(xmlStreamToJSON, EncodedJSON)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

openItem :: String -> IO T.Text
openItem url | not $ "http://" `isPrefixOf` url = TIO.readFile url
openItem url = bracket
    (openTempFile "." "tagsoup.tmp")
    (\(file,_) -> removeFile file)
    $ \(file,hndl) -> do
        hClose hndl
        putStrLn $ "Downloading: " ++ url
        res <- system $ "wget " ++ url ++ " -O " ++ file
        when (res /= ExitSuccess) $ error $ "Failed to download using wget: " ++ url
        src <- TIO.readFile file
        return src

main :: IO ()
main = do
    args <- getArgs
    fileData <- openItem . head $ args
    forM_ (xmlStreamToJSON fileData) TIO.putStrLn
