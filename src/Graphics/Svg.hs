module Graphics.Svg ( loadSvgFile
                    , cssApply 
                    , applyCSSRules
                    , xmlOfDocument
                    , saveXmlFile
                    , documentSize
                    ) where

import Control.Applicative( (<$>) )
import Data.List( foldl' )
import qualified Data.Text as T
import Text.XML.Light.Input( parseXMLDoc )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
import Control.Lens

import Graphics.Svg.Types
import Graphics.Svg.CssTypes
import Graphics.Svg.XmlParser

{-import Graphics.Svg.CssParser-}

loadSvgFile :: FilePath -> IO (Maybe Document)
loadSvgFile filename = do
    fileContent <- readFile filename
    return $ parseXMLDoc fileContent >>= unparseDocument 

saveXmlFile :: FilePath -> Document -> IO ()
saveXmlFile filePath =
    writeFile filePath . ppcTopElement prettyConfigPP . xmlOfDocument

cssDeclApplyer :: DrawAttributes -> CssDeclaration
               -> DrawAttributes 
cssDeclApplyer value (CssDeclaration txt elems) = 
   case lookup txt cssUpdaters of
     Nothing -> value
     Just f -> f value elems
  where
    cssUpdaters = [(T.pack $ _attributeName n, u) |
                            (n, u) <- drawAttributesList]

cssApply :: [CssRule] -> Tree -> Tree
cssApply rules = zipTree go where
  go [] = None
  go ([]:_) = None
  go context@((t:_):_) = t & drawAttr .~ attr'
   where
     matchingDeclarations =
         findMatchingDeclarations rules context
     attr = view drawAttr t
     attr' = foldl' cssDeclApplyer attr matchingDeclarations
   
applyCSSRules :: Document -> Document
applyCSSRules doc = doc
    { _elements = cssApply (_styleRules doc) <$> _elements doc}

