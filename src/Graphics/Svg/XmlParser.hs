{-# LANGUAGE ViewPatterns #-}
module Graphics.Svg.XmlParser where

import Control.Applicative( (<$>) )
import Control.Monad( join )
import Text.XML.Light.Proc( findAttr, elChildren )
import Text.XML.Light.Types( Element( .. )
                           , QName( .. ) )
import qualified Data.Text as T
import Data.Attoparsec.Text( parseOnly, many1 )
import Graphics.Svg.Types
import Graphics.Svg.PathParser
import Graphics.Svg.ColorParser

import Debug.Trace
{-
findAttr :: QName -> Element -> Maybe String
-}

nodeName :: Element -> String
nodeName = qName . elName

attributeFinder :: String -> Element -> Maybe String
attributeFinder str e =
    findAttr QName { qName = str, qURI = Nothing, qPrefix = Nothing }
             e
    
parseDrawAttributes :: Element -> SvgDrawAttributes
parseDrawAttributes e = SvgDrawAttributes
    { _strokeWidth = read <$> attribute "stroke-width"
    , _strokeColor = join $ attribute "stroke" >>= parse "stroke" colorParser
    , _fillColor   = join $ attribute "fill" >>= parse "fill" colorParser
    , _transform   = attribute "transform" >>= parse "trans" transformParser
    }
  where attribute a = attributeFinder a e
        parse v p str = case parseOnly p (T.pack str) of
            Left _ -> Nothing
            Right r -> trace (">> " ++ str ++ " " ++ v ++ show r) $ Just r


unparse :: Element -> SvgTree
unparse e@(nodeName -> "g") =
    Group (parseDrawAttributes e) $ unparse <$> (elChildren e)
unparse e@(nodeName -> "path") =
    Path (parseDrawAttributes e) parsedPath
      where parsedPath = case attributeFinder "d" e of
                Nothing -> []
                Just v -> pathParse $ T.pack v

            pathParse txt = case parseOnly (many1 command) txt of
                Left _ -> []
                Right r -> r


unparse _ = SvgNone

unparseDocument :: Element -> Maybe SvgDocument
unparseDocument e@(nodeName -> "svg") = Just $ SvgDocument 
    { _svgViewBox = (0, 0, 0, 0)
    , _svgElements = unparse <$> elChildren e
    }
unparseDocument _ = Nothing   

