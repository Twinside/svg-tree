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
import Data.Text( Text )
import Graphics.Svg.Types

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
  cssIdOf     :: a -> Text
  cssClassOf  :: a -> Text
  cssNameOf   :: a -> Text
  cssAttribOf :: a -> Text -> Maybe Text

type CssContext a = [[a]]

isDescribedBy :: CssMatcheable a
              => a -> [CssDescriptor] -> Bool
isDescribedBy e = all tryMatch
  where
    tryMatch (OfClass t) = cssClassOf e == t
    tryMatch (OfName  n) = cssNameOf e == n
    tryMatch (OfId    i) = cssIdOf e == i
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
                         => CssContext a -> [CssRule] -> [CssDeclaration]
findMatchingDeclarations context rules =
    concat [cssDeclarations rule
                    | rule <- rules
                    , selector <- cssRuleSelector rule
                    , isMatching context selector ]

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

