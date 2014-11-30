module Graphics.Svg ( loadSvgFile
                    , renderSvgDocument
                    , renderSvg
                    , documentSize
                    ) where

import Text.XML.Light.Input( parseXMLDoc )

import Graphics.Svg.RasterificRender
import Graphics.Svg.Types
import Graphics.Svg.XmlParser
{-import Graphics.Svg.CssParser-}

loadSvgFile :: FilePath -> IO (Maybe Document)
loadSvgFile filename = do
    fileContent <- readFile filename
    return $ parseXMLDoc fileContent >>= unparseDocument 

