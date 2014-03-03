{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.ColorParser( colorParser ) where

import Data.Bits( (.|.), unsafeShiftL )
import Control.Applicative( (<$>), (<$)
                          , (<*>), (<*), (*>)
                          , (<|>)
                          )
import Data.Attoparsec.Text
    ( Number( .. )
    , Parser
    , number
    , string
    , skipSpace
    , satisfy
    , inClass
    , takeWhile1
    , option
    )

import Codec.Picture( PixelRGBA8( .. ) )
import Data.Word( Word8 )
import Graphics.Svg.NamedColors
import qualified Data.Map as M

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ())
                     <* skipSpace

num :: Parser Double
num = skipSpace *> plusMinus <* skipSpace
  where toDouble (I i) = fromIntegral i
        toDouble (D d) = d

        doubleNumber = toDouble <$> number

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

colorParser :: Parser (Maybe PixelRGBA8)
colorParser = none
           <|> (Just <$> rgbColor)
           <|> (string "#" *> (Just <$> (color <|> colorReduced)))
           <|> namedColor
           <|> return (Just black)
  where
    charRange c1 c2 =
        (\c -> fromIntegral $ fromEnum c - fromEnum c1) <$> satisfy (\v -> c1 <= v && v <= c2)
    black = PixelRGBA8 0 0 0 255

    namedColor = do
        str <- takeWhile1 (inClass "a-z")
        return . Just $ M.findWithDefault black str svgNamedColors

    hexChar :: Parser Word8
    hexChar = charRange '0' '9'
           <|> ((+ 10) <$> charRange 'a' 'f')
           <|> ((+ 10) <$> charRange 'A' 'F')

    
    percentToWord v = floor $ v * (255 / 100)

    numPercent = ((percentToWord <$> num) <* string "%")
              <|> (floor <$> num)

    hexByte = (\h1 h2 -> h1 `unsafeShiftL` 4 .|. h2) <$> hexChar <*> hexChar
    color = (\r g b -> PixelRGBA8 r g b 255) <$> hexByte <*> hexByte <*> hexByte
    rgbColor = (\r g b -> PixelRGBA8 r g b 255)
            <$> (string "rgb(" *> numPercent)
            <*> (commaWsp *> numPercent)
            <*> (commaWsp *> numPercent <* skipSpace <* string ")")

    colorReduced =
        (\r g b -> PixelRGBA8 (r * 17) (g * 17) (b * 17) 255) <$> hexChar <*> hexChar <*> hexChar
    none = Nothing <$ string "none"
    

