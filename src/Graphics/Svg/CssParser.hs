{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.CssParser where

import Control.Applicative( (<$>), (<$)
                          , (<*>), (<*), (*>)
                          , (<|>)
                          , many
                          , pure
                          )
import Data.Attoparsec.Text
    ( Number( .. )
    , Parser
    , number
    , string
    , skipSpace
    , letter
    , char
    , digit
    )
import Data.Attoparsec.Combinator
    ( option
    , sepBy
    {-, sepBy1-}
    , many1
    )

import Graphics.Svg.Types
{-import Graphics.Rasterific.Linear( V2( V2 ) )-}
{-import Graphics.Rasterific.Transformations-}

num :: Parser Float
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where toDouble (I i) = fromIntegral i
        toDouble (D d) = d

        doubleNumber = toDouble <$> number

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

commaWsp :: Parser ()
commaWsp = skipSpace *> 
    option () (string "," *> return ())
                     <* skipSpace


{-
stylesheet  : [ CDO | CDC | S | statement ]*;
statement   : ruleset | at-rule;
at-rule     : ATKEYWORD S* any* [ block | ';' S* ];
block       : '{' S* [ any | block | ATKEYWORD S* | ';' S* ]* '}' S*;
ruleset     : selector? '{' S* declaration? [ ';' S* declaration? ]* '}' S*;
selector    : any+;
declaration : property S* ':' S* value;
property    : IDENT;
value       : [ any | block | ATKEYWORD S* ]+;
any         : [ IDENT | NUMBER | PERCENTAGE | DIMENSION | STRING
              | DELIM | URI | HASH | UNICODE-RANGE | INCLUDES
              | DASHMATCH | ':' | FUNCTION S* [any|unused]* ')'
              | '(' S* [any|unused]* ')' | '[' S* [any|unused]* ']'
              ] S*;
unused      : block | ATKEYWORD S* | ';' S* | CDO S* | CDC S*;
-- -}

{-
ident 	[-]?{nmstart}{nmchar}*
name 	{nmchar}+
nmstart 	[_a-z]|{nonascii}|{escape}
nonascii	[^\0-\237]
unicode 	\\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
escape 	{unicode}|\\[^\n\r\f0-9a-f]
nmchar 	[_a-z0-9-]|{nonascii}|{escape}
num 	[0-9]+|[0-9]*\.[0-9]+

IDENT 	{ident}
ATKEYWORD 	@{ident}
STRING 	{string}
BAD_STRING 	{badstring}
BAD_URI 	{baduri}
BAD_COMMENT 	{badcomment}
HASH 	#{name}
NUMBER 	{num}
PERCENTAGE 	{num}%
DIMENSION 	{num}{ident}
URI 	url\({w}{string}{w}\)
|url\({w}([!#$%&*-\[\]-~]|{nonascii}|{escape})*{w}\)
UNICODE-RANGE 	u\+[0-9a-f?]{1,6}(-[0-9a-f]{1,6})?
CDO 	<!--
CDC 	-->
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
-- -}

cssStatement :: Parser CssRule
cssStatement = ruleSet <|> atRule

ident :: Parser String
ident = do
  firstLetter <- letter <|> char '_' <|> char '-'
  (firstLetter:) <$>
      many (letter <|> digit <|> char '_' <|> char '-')

atKeyword :: Parser String
atKeyword = char '@' *> ident <* skipSpace

data CssRule
    = AtQuery String [CssRule]
    deriving (Eq, Show)

atRule :: Parser CssRule
atRule = AtQuery <$> atKeyword <*> many cssStatement

data CssElement
    = CssIdent String
    | CssNumber SvgNumber
    | CssFunction String [CssElement]
    deriving (Eq, Show)

type CssSelector = [CssElement]

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

selector :: Parser CssSelector
selector = many1 anyElem

ruleSet :: Parser CssRule
ruleSet = undefined

