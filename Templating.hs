{-# LANGUAGE Arrows, QuasiQuotes, OverloadedStrings #-}
module Templating where
import Data.Maybe (fromJust)
import Text.XML.HXT.Core
import Control.Arrow.ArrowList
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.DOM.TypeDefs
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (isInfixOf, intercalate)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HM
import Data.String.QQ 
import qualified Data.Vector as V
import Text.Parsec
import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Data.Monoid
import Data.List.Split
import Data.Scientific 
import Expressions

newtype TmplDirective a = TmplDirective a
    deriving Show

data TmplRepeatContext = TmplRepeatContext Text Value
    deriving Show

processTemplate file context = runX (
    readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file
    >>> 
    setTraceLevel 0
    >>>
    process context
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )

processTemplateWithLayout layoutFile file context = runX (
    (
      (\contentXml -> 
          readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] layoutFile
          >>> 
          processTopDown (replaceChildren (constA contentXml) `when` (isElem >>> hasAttr "ng-view"))
      )
      $< (readDocument [withValidate no, withParseHTML yes, withInputEncoding utf8] file)
    )
    >>> 
    setTraceLevel 0
    >>>
    process context
    >>>
    writeDocument [withIndent yes, withOutputHTML, withXmlPi no] "-"
    )


process :: Value -> IOSArrow XmlTree XmlTree
process context = 
    normalTmplProcess context
    >>>
    processTopDown (

      flatten "ng-href" 
      >>> 
      flatten "ng-src" 
    )

normalTmplProcess context = processTopDown (
      templateRepeat context `when` hasTmplAttr "ng-repeat"
      >>> interpolateValues context 
      >>> templateClass context
      >>> templateShow context 
      >>> templateHide context  
      >>> templateBind context
      >>> templateBindHtml context
      >>> templateBindHtmlUnsafe context
    )

------------------------------------------------------------------------
-- general interpolation of {{ }} in text nodes


interpolateValues :: ArrowXml a => Value -> a XmlTree XmlTree
interpolateValues context = 
      ((changeText (interpolateText context)) `when` isText)
      >>>
      (processAttrl (changeAttrValue (interpolateText context)) `when` isElem)
   
interpolateText context = mconcat .  map (evalText context) .  parseText

templateBindBase :: String -> Value -> IOSArrow XmlTree XmlTree
templateBindBase tag context = 
    (
      --txt $< (getAttrValue tag >>> arr (templateEvalToString context) )
      replaceChildren (
        (getAttrValue tag >>> arr (templateEvalToString context) ) >>> xread
      ) >>> removeAttr tag
    ) `when` hasTmplAttr tag

templateBind = templateBindBase "ng-bind"
templateBindHtml = templateBindBase "ng-bind-html"
templateBindHtmlUnsafe = templateBindBase "ng-bind-html-unsafe"

-- ng-bind-html

-- ng-bind-html-unsafe

------------------------------------------------------------------------
-- templateShow
templateShow :: ArrowXml a => Value -> a XmlTree XmlTree
templateShow context = 
    (
      ((\boolVal -> if boolVal then this else none) 
        $< (getAttrValue "ng-show" >>> arr (templateEvalToBool context . T.pack))
      ) >>> removeAttr "ng-show"
    ) `when` hasTmplAttr "ng-show"

-- Not DRY. refactor later
templateHide :: ArrowXml a => Value -> a XmlTree XmlTree
templateHide context = 
    (
      ((\boolVal -> if boolVal then none else this) 
        $< (getAttrValue "ng-hide" >>> arr (templateEvalToBool context . T.pack))
      ) >>> removeAttr "ng-hide"
    ) `when` hasTmplAttr "ng-hide"

flatten :: ArrowXml a => String -> a XmlTree XmlTree
flatten name = processAttrl 
      (changeAttrName (const (mkName $ replacement name)))
      `when` (isElem >>> hasAttr name)
    where replacement = drop 3

templateClass context = 
    (
      ((\newClassNames -> 
        addAttr "class" newClassNames `when` neg (hasName "class")
        `orElse`
        processAttrl (
            changeAttrValue (\old -> mconcat [old, " ", newClassNames]) `when` hasName "class"
          )
              -- addAttr "class" classNames
      ) $< (getAttrValue "ng-class" >>> arr (templateEvalToString context))
      ) >>> removeAttr "ng-class"
    ) `when` hasTmplAttr "ng-class"
     
------------------------------------------------------------------------
-- templateRepeat

templateRepeat :: Value       -- ^ the global context JSON Value
         -> IOSArrow XmlTree XmlTree
templateRepeat context = 
    (templateRepeatContext context $< templateRepeatKeys context) 

templateRepeatKeys :: Value -> IOSArrow XmlTree TmplRepeatContext
templateRepeatKeys outerContext = 
      getAttrValue "ng-repeat" 
      >>> traceValue 2 (show)
      >>> arr parseTmplRepeatExpr
  where parseTmplRepeatExpr :: String -> TmplRepeatContext
        parseTmplRepeatExpr = runParse $ do
          iterVarName <- templateVarName
          spaces >> string "in" >> spaces
          arrayValue <- flip templateExprEval outerContext <$> templateExpr
          return $ TmplRepeatContext (T.pack iterVarName) arrayValue

templateRepeatContext :: Value -> TmplRepeatContext -> IOSArrow XmlTree XmlTree
templateRepeatContext c@(Object context) nrp@(TmplRepeatContext iterVarName repeatContext) = 
    (\iterVar ->
      traceMsg 2 ("* templateRepeatContext with templateRepeatKeys " ++ show nrp) 
      >>>
      removeAttr "ng-repeat" 
      >>>
      let mergedContext = Object $ HM.insert iterVarName iterVar context
      in (

            ( traceMsg 2 ("nested NGREPEAT context: " ++ (debugJSON mergedContext)) 
            >>> normalTmplProcess mergedContext
            )

        )
    ) $< (
            traceMsg 2 ("repeat context " ++ debugJSON repeatContext ++ " for context " ++ debugJSON c)
            >>>
            constL (getList $ repeatContext)
            >>> traceValue 2 show
         )
  -- THIS IS THE BUG. k can be a key path
  where getList :: Value -> [Value]
        getList v = 
            case v of
              Array xs -> V.toList xs
              _ -> []
        -- merge iteration object with general context
templateRepeatContext _ _ = none

------------------------------------------------------------------------

hasTmplAttr :: ArrowXml a => String -> a XmlTree XmlTree
hasTmplAttr attrName = isElem >>> hasAttr attrName


