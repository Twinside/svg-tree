import Graphics.Svg
import System.Environment( getArgs )

{-import Debug.Trace-}


loadRender :: [String] -> IO ()
loadRender [] = putStrLn "not enough arguments"
loadRender [_] = putStrLn "not enough arguments"
loadRender (svgfilename:pngfilename:_) = do
  f <- loadSvgFile svgfilename
  case f of
     Nothing -> putStrLn "Error while loading SVG"
     Just doc -> renderSvgDocument pngfilename doc

main :: IO ()
main = getArgs >>= loadRender

