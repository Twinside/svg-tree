module Graphics.Svg  where

import Graphics.Svg.XmlParser
import Graphics.Svg.Types
import Text.XML.Light.Input( parseXMLDoc )

loadSvgFile :: FilePath -> IO (Maybe SvgDocument)
loadSvgFile filename = do
    fileContent <- readFile filename
    return $ parseXMLDoc fileContent >>= unparseDocument 

