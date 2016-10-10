{-# LANGUAGE OverloadedStrings #-}
import Graphics.Svg( loadSvgFile )
import System.Environment( getArgs )
import Text.Show.Pretty( pPrint )

main :: IO ()
main = do
  (f:_) <- getArgs
  loadSvgFile f >>= pPrint 

