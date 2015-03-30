module Main where
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import Text.XML.HXT.Core

main = do
    [layout,content] <- getArgs
    processTemplateWithLayout layout content "content" >> return ()


processTemplateWithLayout layoutFile contentFile idName = runX (
    (
      (\contentXml -> 
          readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] layoutFile
          >>> 
          processTopDown ((constA contentXml) `when` (isElem >>> hasName "yield" >>> hasAttrValue "id" (== idName)))
      )
      $< (readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] contentFile 
          >>> (deep (isElem >>> hasAttrValue "id" (== idName))) `when` isElem)
    )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )



     
