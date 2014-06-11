{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.CssParser where

import Control.Applicative( (<$>), (<$)
                          , (<*>), (<*), (*>)
                          , (<|>)
                          , many
                          , pure
                          )
import Data.Attoparsec.Text
    ( Parser
    , scientific
    , string
    , skipSpace
    , letter
    , char
    , digit
    )
import qualified Data.Attoparsec.Text as AT

import Data.Attoparsec.Combinator
    ( option
    , sepBy
    {-, sepBy1-}
    , many1
    )

import Graphics.Svg.Types
import Data.Text( Text )
import qualified Data.Text as T
{-import Graphics.Rasterific.Linear( V2( V2 ) )-}
{-import Graphics.Rasterific.Transformations-}

num :: Parser Float
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where doubleNumber = toRational <$> scientific

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

commaWsp :: Parser ()
commaWsp = skipSpace *> 
    option () (string "," *> return ())
                     <* skipSpace


ident :: Parser Text
ident =
  (\f c -> f . T.cons c . T.pack)
        <$> trailingSub
        <*> nmstart <*> nmchar
  where
    trailingSub = option id $ (T.cons '-') <$ char '-'
    underscore = char '_'
    nmstart = letter <|> underscore
    nmchar = many (letter <|> digit <|> underscore <|> char '-')

str :: Parser Text
str = char '"' *> AT.takeWhile (/= '"') <* char '"' <* skipSpace

{-
: 	:
; 	;
{ 	\{
} 	\}
( 	\(
) 	\)
[ 	\[
] 	\]
S 	[ \t\r\n\f]+
COMMENT 	\/\*[^*]*\*+([^/*][^*]*\*+)*\/
FUNCTION 	{ident}\(
INCLUDES 	~=
DASHMATCH 	|
-}

{-  
stylesheet
  : [ CHARSET_SYM STRING ';' ]?
    [S|CDO|CDC]* [ import [ CDO S* | CDC S* ]* ]*
    [ [ ruleset | media | page ] [ CDO S* | CDC S* ]* ]*
  ;
-- -}
{-
operator
  : '/' S* | ',' S*
  ;
-- -}

data CssSelector
  = Nearby          -- ^ '+'
  | DirectChildren  -- ^ '>'
  | AnyElem         -- ^ '*'
  | AllOf [CssSelector]
  | OfClass Text    -- ^ .IDENT
  | OfName  Text    -- ^ IDENT
  | OfId    Text    -- ^ #IDENT
  | OfPseudoClass Text   -- ^ :IDENT (ignore function syntax)
  | WithAttrib Text Text
  deriving (Eq, Show)

-- | combinator: '+' S* | '>' S*
combinator :: Parser CssSelector
combinator = parse <* skipSpace where
  parse = Nearby <$ char '+'
       <|> DirectChildren <$ char '>'

{-
unary_operator
  : '-' | '+'
  ;
-- -}
property :: Parser Text
property = ident <* skipSpace
{-
ruleset
  : selector [ ',' S* selector ]*
    '{' S* declaration? [ ';' S* declaration? ]* '}' S*
  ;
-- -}
{-
selector
  : simple_selector [ combinator selector | S+ [ combinator? selector ]? ]?
  ;
-- -}

selector :: Parser [CssSelector]
selector = (:)
        <$> (AllOf <$> simpleSelector)
        <*> ((:) <$> combinator <*> selector
            <|> next
            <|> return [])
  where
    combOpt :: Parser ([CssSelector] -> [CssSelector])

    combOpt = skipSpace *> (option id $ (:) <$> combinator)
    next :: Parser [CssSelector]
    next = id <$> combOpt <*> selector

simpleSelector :: Parser [CssSelector]
simpleSelector = (:) <$> elementName <*> many whole
              <|> many1 whole
 where
  whole = hash <|> classParser <|> attrib <|> pseudo
  pseudo = char ':' *> (OfPseudoClass <$> ident)
  hash = char '#' *> (OfId <$> ident)
  classParser = char '.' *> (OfClass <$> ident)
  elementName = (OfName <$> ident)
             <|> AnyElem <$ char '*'
  bracket p =
      char '[' *> skipSpace *> p
              <* skipSpace <* char ']' <* skipSpace
  attrib = bracket $
    WithAttrib <$> ident <*> (char '=' *> skipSpace *> (ident <|> str))

{-
declaration
  : property ':' S* expr prio?
  ;
-- -}
{-
prio
  : IMPORTANT_SYM S*
  ;
-- -}
{-
expr
  : term [ operator? term ]*
  ;
-- -}
{-
term
  : unary_operator?
    [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
      TIME S* | FREQ S* ]
  | STRING S* | IDENT S* | URI S* | hexcolor | function
  ;
-- -}
{-
function
  : FUNCTION S* expr ')' S*
  ;
-- -}

atKeyword :: Parser Text
atKeyword = char '@' *> ident <* skipSpace

data CssRule
    = AtQuery String [CssRule]
    deriving (Eq, Show)

data CssElement
    = CssIdent Text
    | CssNumber SvgNumber
    | CssFunction Text [CssElement]
    deriving (Eq, Show)

unitParser :: Parser (Float -> Float)
unitParser =
      (* 1.25) <$ "pt"
  <|> (* 15) <$ "pc"
  <|> (* 3.543307) <$ "mm"
  <|> (* 35.43307) <$ "cm"
  <|> (* 90) <$ "in"
  <|> id <$ "px"
  <|> pure id

unitNumber :: Parser Float
unitNumber = do
  n <- num
  f <- unitParser
  return $ f n

complexNumber :: Parser SvgNumber
complexNumber = do
    n <- num
    let apply f = SvgNum $ f n
    (SvgPercent (n / 100) <$ char '%')
        <|> (SvgEm n <$ string "em")
        <|> (apply <$> unitParser)
        <|> pure (SvgNum n)

anyElem :: Parser CssElement
anyElem = function
   <|> (CssIdent <$> ident)
   <|> (CssNumber <$> complexNumber)
  where
    comma = char ',' <* skipSpace
    function = CssFunction
       <$> ident <* char '('
       <*> (anyElem `sepBy` comma) <* char ')' <* skipSpace

