module Main where
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlArrow

main = do
    -- todo optparse the command line
    let layout = "layout.html"
    let xs = [("content.html", "content"), ("sidebar.html", "sidebar")]
    runX (
      (
        foldl (\acc x -> acc >>> mergeTemplates x) 
            (readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] layout)
            xs
      )
      >>>
      writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
      )


-- mergeTemplates :: ArrowXml a => (String, String) -> a XmlTree XmlTree
mergeTemplates (contentFile, idName) = 
      (\contentXml -> 
          processTopDown ((constA contentXml) `when` (isElem >>> hasName "yield" >>> hasAttrValue "id" (== idName)))
      )
      $< (readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] contentFile 
          >>> (deep (isElem >>> hasAttrValue "id" (== idName))))
    



     
