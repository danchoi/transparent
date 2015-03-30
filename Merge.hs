module Main where
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import Text.XML.HXT.Core

main = do
    [layout,content] <- getArgs
    processTemplateWithLayout layout content >> return ()


processTemplateWithLayout layoutFile contentFile = runX (
    (
      (\contentXml -> 
          readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] layoutFile
          >>> 
          processTopDown ((constA contentXml) `when` (isElem >>> hasName "yield"))
      )
      $< (readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] contentFile 
          >>> (deep (isElem >>> hasAttrValue "id" (== "content"))) `when` isElem)
    )
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )



     
