{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg.ColorParser( colorParser, textureParser ) where

import Data.Bits( (.|.), unsafeShiftL )
import Control.Applicative( (<$>), (<$)
                          , (<*>), (<*), (*>)
                          , (<|>)
                          )
import Data.Attoparsec.Text
    ( Parser
    , string
    , skipSpace
    , satisfy
    , inClass
    , takeWhile1
    , option
    , char
    , digit
    , letter
    , many1
    , scientific
    )

import Data.Scientific( toRealFloat )
import Codec.Picture( PixelRGBA8( .. ) )
import Data.Word( Word8 )
import Graphics.Svg.NamedColors
import Graphics.Svg.Types
import qualified Data.Map as M

commaWsp :: Parser ()
commaWsp = skipSpace *> option () (string "," *> return ())
                     <* skipSpace


num :: Parser Double
num = realToFrac <$> (skipSpace *> plusMinus <* skipSpace)
  where doubleNumber :: Parser Float
        doubleNumber = toRealFloat <$> scientific

        plusMinus = negate <$ string "-" <*> doubleNumber
                 <|> string "+" *> doubleNumber
                 <|> doubleNumber

colorParser :: Parser PixelRGBA8
colorParser = rgbColor
           <|> (string "#" *> (color <|> colorReduced))
           <|> namedColor
  where
    charRange c1 c2 =
        (\c -> fromIntegral $ fromEnum c - fromEnum c1) <$> satisfy (\v -> c1 <= v && v <= c2)
    black = PixelRGBA8 0 0 0 255

    hexChar :: Parser Word8
    hexChar = charRange '0' '9'
           <|> ((+ 10) <$> charRange 'a' 'f')
           <|> ((+ 10) <$> charRange 'A' 'F')

    namedColor = do
      str <- takeWhile1 (inClass "a-z")
      return $ M.findWithDefault black str svgNamedColors
    
    percentToWord v = floor $ v * (255 / 100)

    numPercent = ((percentToWord <$> num) <* string "%")
              <|> (floor <$> num)

    hexByte = (\h1 h2 -> h1 `unsafeShiftL` 4 .|. h2)
           <$> hexChar <*> hexChar

    color = (\r g b -> PixelRGBA8 r g b 255)
         <$> hexByte <*> hexByte <*> hexByte
    rgbColor = (\r g b -> PixelRGBA8 r g b 255)
            <$> (string "rgb(" *> numPercent)
            <*> (commaWsp *> numPercent)
            <*> (commaWsp *> numPercent <* skipSpace <* string ")")

    colorReduced =
        (\r g b -> PixelRGBA8 (r * 17) (g * 17) (b * 17) 255)
        <$> hexChar <*> hexChar <*> hexChar


textureParser :: Parser (Maybe Texture)
textureParser =
  (Just <$> (none
           <|> (TextureRef <$> urlRef)
           <|> (ColorRef <$> colorParser)))
        <|> return Nothing
  where
    urlRef = string "url(" *> skipSpace *>
            char '#' *> many1 (letter <|> digit)
            <* skipSpace <* char ')' <* skipSpace

    none = FillNone <$ string "none"

