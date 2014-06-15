-- | Define the types used to describes CSS elements
module Graphics.Svg.CssTypes
    ( CssSelector( .. )
    , CssRule( .. )
    , CssDeclaration( .. )
    , CssElement( .. )
    ) where

import Codec.Picture( PixelRGBA8 )
import Data.Text( Text )
import Graphics.Svg.Types

data CssSelector
  = Nearby          -- ^ '+'
  | DirectChildren  -- ^ '>'
  | AnyElem         -- ^ '*'
  | AllOf [CssSelector]
  | OfClass Text    -- ^ .IDENT
  | OfName  Text    -- ^ IDENT
  | OfId    Text    -- ^ #IDENT
  | OfPseudoClass Text   -- ^ :IDENT (ignore function syntax)
  | WithAttrib Text Text
  deriving (Eq, Show)

data CssRule
    = CssRule [[CssSelector]] [CssDeclaration]
    deriving (Eq, Show)

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

