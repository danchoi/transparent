{-# LANGUAGE OverloadedStrings, QuasiQuotes, ExistentialQuantification, ScopedTypeVariables #-}
module Expressions where
import Text.Parsec hiding (many, (<|>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Data.List (intersperse)
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
import Data.Functor.Identity (Identity )
import qualified Data.Map.Strict as M

-- see doc for AngularJS expressions https://docs.angularjs.org/guide/expression

data TmplExprTopLevel = TmplExprTopLevel TmplExpr 
    deriving (Show, Eq)

data TmplExpr = TmplKeyPath [JSKey]
            | Or TmplExpr TmplExpr
            | And TmplExpr TmplExpr
            | Neg TmplExpr 
            | Compare String TmplExpr TmplExpr
            -- map is used by templateClass: https://docs.angularjs.org/api/ng/directive/templateClass
            -- A map of class names to boolean values. In the case of a map,
            -- the names of the properties whose values are truthy will be added
            -- as css classes to the element.
            | TmplMap (M.Map String TmplExpr)
            | TmplLiteral Value
      deriving (Show, Eq)

data JSKey = ObjectKey Text 
           | ArrayIndex Int  
           | Method Text [Text]  -- method name and arguments
    deriving (Show, Eq)

data TextChunk = PassThrough String | Interpolation String 
    deriving (Show, Eq)

data ComparableValue = ComparableNumberValue Scientific
                     | ComparableStritemplateValue Text
                     | ComparableBoolValue Bool
                     | ComparableNullValue 
    deriving (Ord, Eq, Show)

type TmplExprParser = ParsecT String () Identity 

symbol s = spaces *> string s <* spaces

templateExprTopLevel = TmplExprTopLevel <$> templateExpr 

templateExpr = do
    templateMap <|> (do
      maybeNeg <- optionMaybe (symbol "!") 
      expr1' <- templateExprTerm
      let expr1 = case maybeNeg of 
                      Just "!" -> Neg expr1'
                      _ -> expr1'
      try (do symbol "&&"; expr2 <- templateExpr; return $ And expr1 expr2) 
       <|> try (do symbol "||"; expr2 <- templateExpr; return $ Or expr1 expr2) 
       <|> try (do op <- comparisonOp; expr2 <- templateExpr; return $ Compare op expr1 expr2) 
       <|> return expr1)


templateMap = do
    char '{' >> spaces
    pairs :: [(String, TmplExpr)] <- sepBy1 ((,) <$> templateVarName <*> (char ':' >> spaces >> templateExpr <* spaces)) (char ',' >> spaces)
    spaces >> char '}' >> spaces
    return $ TmplMap $ M.fromList pairs

comparisonOp = choice $ map (try . symbol) [">=", "<=", "!=", ">", "<", "=="]

templateExprTerm = (char '(' *> templateExpr <* char ')') <|>  templateLiteral <|> templateKeyPath

templateKeyPath :: TmplExprParser TmplExpr
templateKeyPath = do
    ks <- sepBy1 templateKeyPathComponent (char '.' <|> char '[') 
    return $ TmplKeyPath ks 

templateKeyPathComponent :: TmplExprParser JSKey
templateKeyPathComponent = 
    (ArrayIndex . read <$> many1 digit <* char ']') <|> 
    (try templateKeyPathMethodWithArgs) <|> 
    (toJSKey . T.pack <$> templateVarName)

templateVarName = many1 (alphaNum <|> char '$' <|> char '_')

templateLiteral = TmplLiteral <$> (templateNumber <|> templateString <|> templateBool)

-- just integers for now
templateNumber = Number . read <$> many1 digit  

-- dumb simple implementation that does not deal with escaping
templateString = String . T.pack <$> ((char '"' *> many (noneOf "\"") <* char '"') <|> (char '\'' *> many (noneOf "'") <* char '\''))

templateBool = Bool <$> ((try (string "true") *> pure True) <|> (try (string "false") *> pure False))

------------------------------------------------------------------------

-- | function to evaluate a template expression and a object value context
-- e.g. Value -> "item.name" -> "John"
templateEvalToString :: Value -> String -> String
templateEvalToString context exprString = valToString . templateExprEval (runParse templateExpr exprString) $ context

templateEvalToBool :: Value -> Text -> Bool
templateEvalToBool context exprString =
    let expr = runParse templateExpr (T.unpack exprString)
        val = templateExprEval expr context
    in valueToBool val

valueToBool :: Value -> Bool
valueToBool (String "") = False
valueToBool (Bool False) = False
valueToBool Null = False
valueToBool (Bool True) = True -- not strictly necessary pattern
valueToBool _ = True

evalText :: Value -> TextChunk -> String
evalText v (PassThrough s) = s
evalText v (Interpolation s) = templateEvalToString v s

templateEval :: [Text] -> Value -> Value
templateEval keyPath context = templateExprEval (TmplKeyPath $ map toJSKey keyPath) context

templateExprEval :: TmplExpr -> Value -> Value
templateExprEval (TmplLiteral x) _ = x
templateExprEval (TmplKeyPath ks) v = templateEvaluate ks v
templateExprEval (Or x y) v       = 
      let vx = templateExprEval x v 
      in if (valueToBool vx) then vx else (templateExprEval y v)
templateExprEval (And x y) v      = 
      let vx = templateExprEval x v
      in if (valueToBool vx) then (templateExprEval y v) else (Bool False)
templateExprEval (Neg x) v        = 
      let vx = templateExprEval x v
      in case vx of 
          Null  -> Bool True
          Bool False -> Bool True
          String "" -> Bool True   -- ? is this right?
          _ -> Bool False
templateExprEval (TmplMap ngmap) v =
      let xs :: [(String, TmplExpr)] = M.toList ngmap
          trueKeys = [k | (k,expr) <- xs, (valueToBool $ templateExprEval expr v)]
      -- return all true keys concatenated interspersed with spaces
      in case trueKeys of
            [] -> Null
            xs -> String $ mconcat $ intersperse " " $ map T.pack xs
templateExprEval (Compare op x y) v      = 
      let vx = comparableValue $ templateExprEval x v
          vy = comparableValue $ templateExprEval y v
      in case op of   
            ">" -> Bool $ vx > vy
            "<" -> Bool $ vx < vy
            ">=" -> Bool $ vx >= vy
            "<=" -> Bool $ vx <= vy
            "==" -> Bool $ vx == vy
            "!=" -> Bool $ vx /= vy

comparableValue :: Value -> ComparableValue
comparableValue (Number x) = ComparableNumberValue x
comparableValue (String x) = ComparableStritemplateValue x
comparableValue (Bool x) = ComparableBoolValue x
comparableValue Null = ComparableNullValue 
comparableValue x = error $ "can't make comparable value for " ++ show x


-- evaluates the a JS key path against a Value context to a leaf Value
templateEvaluate :: [JSKey] -> Value -> Value
templateEvaluate [] x@(String _) = x
templateEvaluate [] x@Null = x
templateEvaluate [] x@(Number _) = x
templateEvaluate [] x@(Bool _) = x
templateEvaluate [] x@(Object _) = x
templateEvaluate [] x@(Array _) = x
templateEvaluate ((ObjectKey key):(Method "length" []):[]) (Object s) = 
    case (HM.lookup key s) of
        (Just (Array vs)) -> toJSON $ V.length vs
        _ -> Null
templateEvaluate ((ObjectKey key):(Method "join" [separator]):[]) (Object s) = 
    case (HM.lookup key s) of
        (Just (Array vs)) -> String $ mconcat $ intersperse separator $ map (T.pack . valToString) $ V.toList vs
        _ -> Null
templateEvaluate ((ObjectKey key):xs) (Object s) = templateEvaluate xs (HM.lookupDefault Null key s)
templateEvaluate ((ArrayIndex idx):xs) (Array v) = case V.length v > 0 of
          True -> templateEvaluate xs (v V.! idx)
          False -> Null
templateEvaluate _ _ = Null


-- CHANGE TO PARSEC
toJSKey :: Text -> JSKey
toJSKey "length" = Method "length" []
toJSKey x = ObjectKey x

templateKeyPathMethodWithArgs :: TmplExprParser JSKey
templateKeyPathMethodWithArgs = do
    methodName <- T.pack <$> templateVarName
    char '('
    -- simplistic argument parser. 
    -- Messes up in an argument is a string with a comma in it.
    args <- pStritemplateMethodArgument `sepBy1` (spaces >> char ',' >> spaces)
    char ')'
    return $ Method methodName args 

pStritemplateMethodArgument :: TmplExprParser Text
pStritemplateMethodArgument = do
    spaces  
    arg <- between (char '\'') (char '\'') (many1 (noneOf "'")) 
           <|> between (char '"') (char '"') (many1 (noneOf "\""))
    return . T.pack $ arg


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
parseText = runParse (many templateTextChunk) 

parseKeyExpr :: String -> [JSKey]
parseKeyExpr s = 
      let (TmplKeyPath ks) = runParse templateKeyPath s
      in ks

templateTextChunk :: TmplExprParser TextChunk
templateTextChunk = interpolationChunk <|> passThroughChunk

interpolationChunk = do
    try (string "{{")
    spaces
    xs <- manyTill anyChar (lookAhead $ try (string "}}"))
    spaces
    string "}}"
    return $ Interpolation xs

passThroughChunk = PassThrough <$> passThrough

passThrough = do
    -- a lead single { char. This is guaranteed not to be part of {{
    t <- optionMaybe (string "{") 
    xs <- many1 (noneOf "{") 
    x <- (eof *> pure "{{") <|> lookAhead (try (string "{{") <|> string "{")
    res <- case x of 
              "{{" -> return []
              "{" -> ('{':) <$> passThrough 
    return $ (fromMaybe "" t) ++ xs ++ res

-- for debugging
debugJSON = B.unpack . encode 

jsonToValue :: B.ByteString -> Value
jsonToValue = fromJust . decode

------------------------------------------------------------------------
-- Tests

t = runTestTT tests

testContext1      = jsonToValue  [s|{"item":"apple","another":10}|]
testContext2      = jsonToValue  [s|{"item":{"name":"apple"}}|]
testContext3      = jsonToValue  [s|{"items":[1,2,3]}|]
testContext4      = jsonToValue  [s|{"item":{"active":false,"canceled":true}}|]
testContext5      = jsonToValue  [s|{"person":{"species":"hobbit"}}|]

tests = test [
    "parseKeyExpr"          ~: [ObjectKey "item"]   @=?   parseKeyExpr "item"
  , "templateEvalToString"        ~: "apple"              @=?   templateEvalToString testContext1 "item" 
  , "templateEvalToString2"       ~: "apple"              @=?   templateEvalToString testContext2 "item.name" 
  , "parse array keypath"   ~: [ObjectKey "items",ArrayIndex 1]       @=?   parseKeyExpr "items[1]" 
  , "eval array index"      ~: "2"                  @=?   templateEvalToString testContext3 "items[1]" 
  , "array index"           ~: "2"                  @=?   templateEvalToString testContext3 "items[1]" 
  , "parse ng map"          ~: TmplMap (M.fromList [("testKey",TmplKeyPath [ObjectKey "item",ObjectKey "name"])])
                               @=? runParse templateExpr "{testKey: item.name}"
  , "parse ng map 2"        ~: TmplMap (M.fromList [("dwarf",Compare "==" (TmplKeyPath [ObjectKey "person",ObjectKey "species"]) (TmplLiteral (String "dwarf"))),("hobbit",Compare "==" (TmplKeyPath [ObjectKey "person",ObjectKey "species"]) (TmplLiteral (String "hobbit")))])
                               @=? runParse templateExpr "{dwarf: person.species == 'dwarf', hobbit: person.species == 'hobbit'}"

  , "parse length method"   ~: [ObjectKey "items",Method "length" []] @=?   parseKeyExpr "items.length" 
  , "parse join method with arg"   
                            ~: [ObjectKey "items",Method "join" [","]] 
                            @=?   parseKeyExpr "items.join(\",\")" 
  , "parse join method with arg, single quotes"   
                            ~: [ObjectKey "items",Method "join" [","]] 
                            @=?   parseKeyExpr "items.join(',')" 
  , "eval length method"    ~: "3"                  
                            @=?   templateEvalToString testContext3 "items.length" 
  , "eval  join method with arg"   
                            ~: "1,2,3"
                            @=? templateEvalToString testContext3 "items.join(\",\")" 
  , "parse ngexpr 1"        ~: TmplKeyPath [ObjectKey "test"]  
                               @=? runParse templateExpr "test"
  , "parse ngexpr 2"        ~: (Or (TmplKeyPath [ObjectKey "test"]) (TmplKeyPath [ObjectKey "test2"]))
                               @=? runParse templateExpr "test || test2"
  , "parse ngexpr 3"        ~: (Or (TmplKeyPath [ObjectKey "test"]) (TmplKeyPath [ObjectKey "test2"]))
                               @=? runParse templateExpr "(test || test2)"
  , "parse ngexpr 4"        ~:  
        And (Or (TmplKeyPath [ObjectKey "test1"]) (TmplKeyPath [ObjectKey "test2"])) (TmplKeyPath [ObjectKey "test3"])
        @=? runParse templateExpr "(test1 || test2) && test3"
  , "parse literal"         ~: TmplLiteral (Number 2)  @=? runParse templateExpr "2"
  , "parse negation"        ~: Neg (TmplKeyPath [ObjectKey "test"]) @=? runParse templateExpr "!test"
  , "parse comparison"      ~: Compare ">" (TmplKeyPath [ObjectKey "test"]) (TmplKeyPath [ObjectKey "test2"])
                               @=? runParse templateExpr "test > test2"
  , "parse comparison with literal" ~: 
                               Compare "==" (TmplKeyPath [ObjectKey "test"]) (TmplLiteral (Number 1))
                               @=? runParse templateExpr "test == 1"
  , "parse top level ng expr" ~: 
                               TmplExprTopLevel (TmplKeyPath [ObjectKey "item",ObjectKey "price"]) 
                               @=? runParse templateExprTopLevel "item.price | number:2"
  , "disjunction left"      ~: "apple"               @=? templateEvalToString testContext1 "item || another" 
  , "disjunction right"     ~: "10"                  @=? templateEvalToString testContext1 "blah || another" 
  , "disjunction in parens" ~: "apple"               @=? templateEvalToString testContext2 "(item.color || item.name)" 
  , "length"                ~: Number 3              @=? templateEval ["items","length"] testContext3 

  , "ngmap eval 1"          ~: "canceled"            @=? templateEvalToString testContext4 "{active: item.active, canceled:item.canceled}"
  , "ngmap eval 2"          ~: "hobbit"              @=? templateEvalToString testContext5 "{dwarf: person.species == 'dwarf', hobbit: person.species == 'hobbit'}"

  , "compare length == again"      ~: Bool True
                               @=? templateExprEval (runParse templateExpr "items.length == 3") testContext3
  , "compare length != "      ~: Bool False
                               @=? templateExprEval (runParse templateExpr "items.length != 3") testContext3
  , "compare length >"      ~: Bool True
                               @=? templateExprEval (runParse templateExpr "items.length > 1") testContext3
  , "compare length >="     ~: Bool True
                               @=? templateExprEval (runParse templateExpr "items.length >= 1") testContext3
  , "compare to string"     ~: "true"
                               @=? templateEvalToString testContext1 "item == 'apple'"
  , "text chunk 1"          ~: Interpolation "test"  @=? runParse interpolationChunk "{{test}}"
  , "text chunk 2"          ~: PassThrough "test"    @=? runParse passThroughChunk "test"
  , "text chunk 3"          ~: PassThrough " test"   @=? runParse templateTextChunk " test"
  , "text chunk 4"          ~: " test"               @=? runParse passThrough " test"
  , "text chunks"           ~: [PassThrough " test ",Interpolation "test2",PassThrough " test"] @=? parseText " test {{test2}} test"
  , "text empty"            ~: [] @=? parseText ""

             ]




