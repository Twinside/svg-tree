import Data.Monoid
import Criterion
import Criterion.Main
import Criterion.Config
import Control.Applicative( (<$>) )
import Graphics.Text.TrueType( emptyFontCache )
import Graphics.Svg

myConfig :: Config
myConfig = mempty { cfgSamples = Last $ Just 20 }

main :: IO ()
main = do
  f <- loadSvgFile "test/tiger.svg"
  {-cache <- loadCreateFontCache-}
  case f of
     Nothing -> putStrLn "Error while loading SVG"
     Just doc -> do
       let loader = fst <$> renderSvgDocument emptyFontCache Nothing doc
       defaultMainWith myConfig (return ())
            [ bench "Tiger render" $ whnfIO loader ]

