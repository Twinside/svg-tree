{-# LANGUAGE ViewPatterns #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>) )
import Control.Monad( join )
import Data.Monoid( Monoid( .. ) )
import Data.Function( on )
import Codec.Picture( PixelRGBA8( .. ) )
import Text.XML.Light.Proc( findAttr, elChildren )
import Text.XML.Light.Types( Element( .. )
                           , QName( .. ) )
import qualified Data.Text as T
import Data.Attoparsec.Text( parseOnly )
import Graphics.Svg.Types
import Graphics.Svg.PathParser

{-
findAttr :: QName -> Element -> Maybe String
-}

nodeName :: Element -> String
nodeName = qName . elName

data SvgTree
    = SvgNone
    | Group SvgDrawAttributes [SvgTree]
    | Path SvgDrawAttributes SvgPath
    deriving (Eq, Show)

data SvgDrawAttributes = SvgDrawAttributes
    { _strokeWidth :: Maybe Float
    , _strokeColor :: Maybe PixelRGBA8
    , _fillColor   :: Maybe PixelRGBA8
    , _transform   :: Maybe Transform
    }
    deriving (Eq, Show)

mayRight :: Maybe a -> Maybe a -> Maybe a
mayRight _ b@(Just _) = b
mayRight a Nothing = a

instance Monoid SvgDrawAttributes where
    mempty = SvgDrawAttributes Nothing Nothing Nothing Nothing
    mappend a b = SvgDrawAttributes
        { _strokeWidth = (mayRight `on` _strokeWidth) a b
        , _strokeColor =  (mayRight `on` _strokeColor) a b
        , _fillColor =  (mayRight `on` _fillColor) a b
        , _transform =  (mayRight `on` _transform) a b
        }

parseDrawAttributes :: Element -> SvgDrawAttributes
parseDrawAttributes e = SvgDrawAttributes
    { _strokeWidth = read <$> attribute "stroke-width"
    , _strokeColor = join $ attribute "stroke" >>= parse colorParser
    , _fillColor   = join $ attribute "fill" >>= parse colorParser
    , _transform   = attribute "transform" >>= parse transformParser
    }
  where attribute a =
            findAttr QName { qName = a, qURI = Nothing, qPrefix = Nothing } e

        parse p str = case parseOnly p (T.pack str) of
            Left _ -> Nothing
            Right r -> Just r

unparse :: Element -> SvgTree
unparse e@(nodeName -> "g") =
    Group (parseDrawAttributes e) $ unparse <$> (elChildren e)
unparse e@(nodeName -> "path") =
    SvgNone
unparse _ = SvgNone


