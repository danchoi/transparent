{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Ng.Expressions where
import Text.Parsec hiding (many, (<|>))
import Data.Maybe (fromJust)
import Data.Monoid
import Data.List.Split
import Data.Scientific 
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative 
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as B
import Test.HUnit 
import Data.String.QQ


-- TODO use this expression data type instead of JSKey directly

data NgExpr = NgKeyPath [JSKey]
            | Or NgExpr NgExpr
            | And NgExpr NgExpr
            | Neg NgExpr 
      deriving (Show, Eq)

data JSKey = ObjectKey Text | ArrayIndex Int  | Method Text
    deriving (Show, Eq)

data TextChunk = PassThrough String | Interpolation String 
    deriving (Show, Eq)


symbol s = spaces *> string s <* spaces

ngExpr = do
    maybeNeg <- optionMaybe (symbol "!") 
    expr1' <- ngExprTerm
    let expr1 = case maybeNeg of 
                    Just "!" -> Neg expr1'
                    _ -> expr1'
    try (do symbol "&&"; expr2 <- ngExpr; return $ And expr1 expr2) 
     <|> try (do symbol "||"; expr2 <- ngExpr; return $ Or expr1 expr2) 
     <|> return expr1

ngExprTerm = (char '(' *> ngExpr <* char ')') <|> ngKeyPath'

ngKeyPath' = NgKeyPath <$> ngKeyPath 

ngKeyPath = do
    ks <- sepBy1 ngVarName (char '.') 
    return $ map (toJSKey . T.pack) ks

-- TODO handle filters someone, maybe with externally supplied shell program
-- e.g. note.title | truncate:100

------------------------------------------------------------------------


-- | function to evaluate an ng-expression and a object value context
-- e.g. "item.name" -> (Object ...) -> "John"

-- TODO Change to NgExpr evaluation
ngEvalToString :: Value -> String -> String
ngEvalToString context keyExpr = valToString . ngEvaluate (parseKeyExpr keyExpr) $ context

-- TODO Change to NgExpr evaluation
ngEvalToBool :: Value -> Text -> Bool
ngEvalToBool context keyExpr =
    let keys = toJSKeyFromTextPath keyExpr
        val = ngEvaluate keys context
    in valueToBool val

valueToBool :: Value -> Bool
valueToBool (String "") = False
valueToBool (Bool False) = False
valueToBool Null = False
valueToBool (Bool True) = True -- not strictly necessary pattern
valueToBool _ = True

evalText :: Value -> TextChunk -> String
evalText v (PassThrough s) = s
evalText v (Interpolation s) = ngEvalToString v s

-- convenience function around ngEvaluate which takes a key
ngEval :: [Text] -> Value -> Value
ngEval keyPath context = ngEvaluate (map toJSKey keyPath) context

-- evaluates the a JS key path against a Value context to a leaf Value
ngEvaluate :: [JSKey] -> Value -> Value
ngEvaluate [] x@(String _) = x
ngEvaluate [] x@Null = x
ngEvaluate [] x@(Number _) = x
ngEvaluate [] x@(Bool _) = x
ngEvaluate [] x@(Object _) = x
ngEvaluate [] x@(Array _) = x
ngEvaluate ((ObjectKey key):(Method "length"):[]) (Object s) = 
    case (HM.lookup key s) of
        (Just (Array vs)) -> toJSON $ V.length vs
        -- x -> error $ "length lookup " ++ show key ++ " found: " ++ show x
        _ -> Null
ngEvaluate ((ObjectKey key):xs) (Object s) = ngEvaluate xs (HM.lookupDefault Null key s)
ngEvaluate ((ArrayIndex idx):xs) (Array v)  = ngEvaluate [] $ v V.! idx
ngEvaluate _ _ = Null

-- TODOO key may have ngFILTER appended. Just ignore it.
-- Move this to the parse

toJSKeyFromTextPath :: Text -> [JSKey]
toJSKeyFromTextPath p = map toJSKey . T.splitOn "." $ p

-- TODO translate [1] expression
-- CHANGE TO PARSEC
toJSKey :: Text -> JSKey
toJSKey "length" = Method "length"
toJSKey x = ObjectKey x

valToString :: Value -> String
valToString (String x) = T.unpack x
valToString Null = ""
valToString (Bool True) = "true"
valToString (Bool False) = "false"
valToString (Number x) = 
    case floatingOrInteger x of
        Left float -> show float
        Right int -> show int
valToString x = debugJSON x

-- parse String to find interpolation expressions


runParse parser inp =
  case Text.Parsec.parse parser "" inp of
    Left x -> error $ "parser failed: " ++ show x
    Right xs -> xs

parseText :: String -> [TextChunk]
parseText = runParse (many ngTextChunk) 

parseKeyExpr :: String -> [JSKey]
parseKeyExpr = runParse ngKeyPath


ngVarName = many1 (alphaNum <|> char '$' <|> char '_')

-- ngTextChunk :: Stream s m Char => ParsecT s u m TextChunk
ngTextChunk =   
    (Interpolation <$> (string "{{" *> many1 (noneOf "}") <* string "}}"))
    <|> (PassThrough <$> (many1 (noneOf "{")))

-- for debugging
debugJSON = B.unpack . encode 

jsonToValue :: B.ByteString -> Value
jsonToValue = fromJust . decode

------------------------------------------------------------------------
-- Tests

runTests = runTestTT tests

testContext1      = jsonToValue  [s|{"item":"apple","price":10}|]
testContext2      = jsonToValue  [s|{"item":{"name":"apple"}}|]
testContext3      = jsonToValue  [s|{"items":[1,2,3]}|]

tests = test [
    "parseKeyExpr"          ~:  [ObjectKey "item"]   @=?   parseKeyExpr "item"
  , "ngEvalToString"        ~:  "apple"              @=?   ngEvalToString testContext1 "item" 
  , "ngEvalToString2"       ~:  "apple"              @=?   ngEvalToString testContext2 "item.name" 
  , "length method"         ~:  "3"                  @=?   ngEvalToString testContext3 "items.length" 
  , "parse ngexpr 1"        ~:  NgKeyPath [ObjectKey "test"]  
                                @=? runParse ngExpr "test"
  , "parse ngexpr 2"        ~:  (Or (NgKeyPath [ObjectKey "test"]) (NgKeyPath [ObjectKey "test2"]))
                                @=? runParse ngExpr "test || test2"
  , "parse ngexpr 3"        ~:  (Or (NgKeyPath [ObjectKey "test"]) (NgKeyPath [ObjectKey "test2"]))
                                @=? runParse ngExpr "(test || test2)"
  , "parse ngexpr 4"        ~:  
      And (Or (NgKeyPath [ObjectKey "test1"]) (NgKeyPath [ObjectKey "test2"])) (NgKeyPath [ObjectKey "test3"])
      @=? runParse ngExpr "(test1 || test2) && test3"
  , "parse negation"        ~:  Neg (NgKeyPath [ObjectKey "test"]) @=? runParse ngExpr "!test"
  , "disjunction"           ~:  "10"                 @=?   ngEvalToString testContext1 "item.color || item.price" 
  , "disjunction in parens" ~:  "10"                 @=?   ngEvalToString testContext1 "(item.color || item.price)" 

             ]




