{-# LANGUAGE OverloadedStrings #-}
-- | Define the types used to describes CSS elements
module Graphics.Svg.CssTypes
    ( CssSelector( .. )
    , CssRule( .. )
    , CssDescriptor( .. )
    , CssDeclaration( .. )
    , CssElement( .. )

    , CssMatcheable( .. )
    , CssContext
    , Number( .. )
    , serializeNumber
    , findMatchingDeclarations
    , tserialize
    ) where

import Data.Monoid( mconcat, (<>) )
import Data.List( intersperse )
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import Text.Printf

import Codec.Picture( PixelRGBA8( .. ) )
{-import Debug.Trace-}

class TextBuildable a where
    tserialize :: a -> TB.Builder

data CssDescriptor
  = OfClass T.Text    -- ^ .IDENT
  | OfName  T.Text    -- ^ IDENT
  | OfId    T.Text    -- ^ #IDENT
  | OfPseudoClass T.Text   -- ^ :IDENT (ignore function syntax)
  | AnyElem         -- ^ '*'
  | WithAttrib T.Text T.Text
  deriving (Eq, Show)

instance TextBuildable CssDescriptor where
  tserialize d = case d of
      OfClass c -> si '.' <> ft c
      OfName  n -> ft n
      OfId    i -> si '#' <> ft i
      OfPseudoClass c -> si '#' <> ft c
      AnyElem -> si '*'
      WithAttrib a b -> mconcat [si '[', ft a, si '=', ft b, si ']']
     where
      ft = TB.fromText 
      si = TB.singleton

-- | Define complex selector.
data CssSelector
  = Nearby          -- ^ Correspond to the `+` CSS selector.
  | DirectChildren  -- ^ Correspond to the `>` CSS selectro.
  | AllOf [CssDescriptor] -- ^ Grouping construct.
  deriving (Eq, Show)

instance TextBuildable CssSelector where
  tserialize s = case s of
      Nearby -> si '+'
      DirectChildren -> si '>'
      AllOf lst ->
        mconcat . intersperse (si ' ') $ map tserialize lst
    where
      si = TB.singleton

data CssRule = CssRule
    { cssRuleSelector :: ![[CssSelector]]
    , cssDeclarations :: ![CssDeclaration]
    }
    deriving (Eq, Show)

instance TextBuildable CssRule where
  tserialize (CssRule selectors decl) =
      mconcat tselectors
                 <> ft " {\n"
                 <> mconcat (fmap tserializeDecl decl)
                 <> ft "}\n"
     where
      ft = TB.fromText 
      tserializeDecl d = ft "  " <> tserialize d <> ft ";\n"
      tselectors =
          intersperse (ft ",\n") . fmap tserialize $ concat selectors

class CssMatcheable a where
  cssIdOf     :: a -> Maybe T.Text
  cssClassOf  :: a -> Maybe T.Text
  cssNameOf   :: a -> T.Text
  cssAttribOf :: a -> T.Text -> Maybe T.Text

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
    concat [cssDeclarations rule
                    | rule <- rules
                    , selector <- cssRuleSelector rule
                    , isMatching context $ reverse selector ]

data CssDeclaration
    = CssDeclaration T.Text [[CssElement]]
    deriving (Eq, Show)

instance TextBuildable CssDeclaration where
  tserialize (CssDeclaration n elems) =
      mconcat $ ft n : ft ": " : intersperse (si ' ') finalElems
     where
      finalElems = map tserialize (concat elems)
      ft = TB.fromText 
      si = TB.singleton


-- | Encode complex number possibly dependant to the current
-- render size.
data Number
  = Num Float       -- ^ Simple coordinate in current user coordinate.
  | Em Float        -- ^ Number relative to the current font size.
  | Percent Float   -- ^ Number relative to the current viewport size.
  deriving (Eq, Show)

-- | Encode the number to string which can be used in a
-- CSS or a svg attributes.
serializeNumber :: Number -> String
serializeNumber n = case n of
    Num c -> printf "%g" c
    Em cc -> printf "%gem" cc
    Percent p -> printf "%d%%" (floor $ 100 * p :: Int)

instance TextBuildable Number where
   tserialize = TB.fromText . T.pack . serializeNumber

data CssElement
    = CssIdent     !T.Text
    | CssString    !T.Text
    | CssReference !T.Text 
    | CssNumber    !Number
    | CssColor     !PixelRGBA8
    | CssFunction  !T.Text ![CssElement]
    | CssOpComa
    | CssOpSlash
    deriving (Eq, Show)

instance TextBuildable CssElement where
  tserialize e = case e of
    CssIdent    n -> ft n
    CssString   s -> si '"' <> ft s <> si '"'
    CssReference r -> si '#' <> ft r
    CssNumber   n -> tserialize n
    CssColor  (PixelRGBA8 r g b _) ->
      ft . T.pack $ printf  "#%02X%02X%02X" r g b
    CssFunction t els -> mconcat $ ft t : si '(' : args ++ [si ')']
        where args = intersperse (ft ", ") (map tserialize els)
    CssOpComa -> si ','
    CssOpSlash -> si '/'
    where
      ft = TB.fromText 
      si = TB.singleton

