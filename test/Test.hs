{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Main where

import Text.XML.JSON.StreamingXmlToJson(xmlStreamToJSON)
import Data.Aeson(decode, Value)
import Data.Maybe(isJust)
import Test.QuickCheck(Arbitrary, arbitrary, oneof, choose, elements, sample, Gen)
import Test.QuickCheck.All(quickCheckAll, verboseCheckAll)
import Data.ByteString.Lazy.Char8(pack, unpack)
import Control.Monad(liftM, replicateM)

import Text.XML.HXT.DOM.ShowXml(xshow)
import Text.XML.HXT.DOM.TypeDefs(XmlTree, XNode(..), mkName)
import Data.Tree.NTree.TypeDefs(NTree(..))

import Debug.Trace(traceShowId)

newtype XmlChar = XmlChar { toChar :: Char }
instance Arbitrary XmlChar where
    arbitrary = elements $ map XmlChar (['A'..'Z'] 
                                        ++ ['a' .. 'z'] 
                                        ++ ['0' .. '9']
                                        ++ "_")

unpack' :: [XmlChar] -> String
unpack' = map toChar

arbitraryText = liftM unpack' (arbitrary :: Gen [XmlChar]) 

arbitrary' :: Int -> Gen (NTree XNode)
arbitrary' n = do 
  text <- arbitraryText
  textAttr <- arbitraryText
  let rootNode = XTag (mkName "root") rootAttrs
      rootAttrs = [NTree (XAttr $ mkName ("_" ++ textAttr)) []]
  children <- if n < 3 
              then do 
                x <- choose (0, 10)
                replicateM x (arbitrary' (n + 1))
              else return []
  return (NTree rootNode children)

instance Arbitrary (NTree XNode) where
    arbitrary = arbitrary' 0 
                
prop_xmlStreamToJson_valid :: XmlTree -> Bool
prop_xmlStreamToJson_valid x = isJust decodedJson
    where json = pack . concat . xmlStreamToJSON . xshow $ [x]
          decodedJson = decode json :: Maybe Value

return []
main = $quickCheckAll
