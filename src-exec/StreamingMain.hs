module Main  where

import Control.Exception
import Control.Monad
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.IO
import System.Environment (getArgs)
import Text.XML.JSON.StreamingXmlToJson(xmlStreamToJSON)

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
        return src

main :: IO ()
main = do
    args <- getArgs
    fileData <- openItem . head $ args
    forM_ (xmlStreamToJSON fileData) putStrLn
