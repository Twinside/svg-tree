-- | Define the types used to describes CSS elements
module Graphics.Svg.CssTypes
    ( CssSelector( .. )
    , CssRule( .. )
    , CssDescriptor( .. )
    , CssDeclaration( .. )
    , CssElement( .. )

    , CssMatcheable( .. )
    , CssContext
    , findMatchingDeclarations
    ) where

import Codec.Picture( PixelRGBA8 )
import Data.Monoid( Last( .. ) )
import Data.Text( Text )
import Control.Lens( view )
import qualified Data.Text as T
import Graphics.Svg.Types
{-import Debug.Trace-}

data CssDescriptor
  = OfClass Text    -- ^ .IDENT
  | OfName  Text    -- ^ IDENT
  | OfId    Text    -- ^ #IDENT
  | OfPseudoClass Text   -- ^ :IDENT (ignore function syntax)
  | AnyElem         -- ^ '*'
  | WithAttrib Text Text
  deriving (Eq, Show)

data CssSelector
  = Nearby          -- ^ '+'
  | DirectChildren  -- ^ '>'
  | AllOf [CssDescriptor]
  deriving (Eq, Show)

data CssRule = CssRule
    { cssRuleSelector :: ![[CssSelector]]
    , cssDeclarations :: ![CssDeclaration]
    }
    deriving (Eq, Show)

class CssMatcheable a where
  cssIdOf     :: a -> Maybe Text
  cssClassOf  :: a -> Maybe Text
  cssNameOf   :: a -> Text
  cssAttribOf :: a -> Text -> Maybe Text

instance CssMatcheable SvgTree where
  cssAttribOf _ _ = Nothing
  cssClassOf = fmap T.pack . getLast . view (drawAttr . attrClass)
  cssIdOf = fmap T.pack . view (drawAttr . attrId)
  cssNameOf = nameOfTree

type CssContext a = [[a]]

isDescribedBy :: CssMatcheable a
              => a -> [CssDescriptor] -> Bool
isDescribedBy e = all tryMatch
  where
    tryMatch (OfClass t) = cssClassOf e == Just t
    tryMatch (OfId    i) = cssIdOf e == Just i
    tryMatch (OfName  n) = cssNameOf e == n
    tryMatch (OfPseudoClass _) = False
    tryMatch (WithAttrib a v) = cssAttribOf e a == Just v
    tryMatch AnyElem = True

isMatching :: CssMatcheable a
           => CssContext a -> [CssSelector] -> Bool
isMatching = go where
  go  _ [] = True
  go []  _ = False
  go ((_ : near):upper) (Nearby : rest) = go (near:upper) rest
  go ((e:_):upper) (DirectChildren:AllOf descr:rest)
    | isDescribedBy e descr = go upper rest
  go _ (DirectChildren:_) = False
  go ((e:_):upper) (AllOf descr : rest)
    | isDescribedBy e descr = go upper rest
    | otherwise = False
  go (_:upper) selector = go upper selector

findMatchingDeclarations :: CssMatcheable a
                         => [CssRule] -> CssContext a -> [CssDeclaration]
findMatchingDeclarations rules context =
    concat [cssDeclarations $ rule
                    | rule <- rules
                    , selector <- cssRuleSelector rule
                    , isMatching context $ reverse selector ]

data CssDeclaration
    = CssDeclaration Text [[CssElement]]
    deriving (Eq, Show)

data CssElement
    = CssIdent    !Text
    | CssString   !Text
    | CssNumber   !SvgNumber
    | CssColor    !PixelRGBA8
    | CssFunction !Text ![CssElement]
    | CssOpComa
    | CssOpSlash
    deriving (Eq, Show)

