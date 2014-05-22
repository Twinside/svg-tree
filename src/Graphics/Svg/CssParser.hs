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
    , sepBy1
    , many1
    )

import Linear( V2( V2 ) )
import Graphics.Svg.Types
import Graphics.Rasterific.Transformations

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
  init <- letter <|> char '_' <|> char '-'
  (init:) <$> many (letter <|> digit <|> char '_' <|> char '-')

atKeyword :: Parser String
atKeyword = char '@' *> ident <* skipSpace

data CssRule
    = AtQuery String [CssRule]
    deriving (Eq, Show)

atRule :: Parser CssRule
atRule = AtQuery <$> atKeyword <*> many cssStatement

data CssElement
    = CssIdent String
    | CssNumber CssNumber
    | CssFunction String [CssElement]
    deriving (Eq, Show)

type CssSelector = [CssElement]

data CssNumber
    = Percentage Number
    | Naked Number
    | Named Number String
    deriving (Eq, Show)

complexNumber :: Parser CssNumber
complexNumber = do
    n <- number
    (Percentage n <$ char '%')
        <|> (Named n <$> ident)
        <|> pure (Naked n)


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

{-  
"1pt" equals "1.25px" (and therefore 1.25 user units)
"1pc" equals "15px" (and therefore 15 user units)
"1mm" would be "3.543307px" (3.543307 user units)
"1cm" equals "35.43307px" (and therefore 35.43307 user units)
"1in" equals "90px" (and therefore 90 user units)
-}
