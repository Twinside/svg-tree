import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import Graphics.Svg
import Data.List( isSuffixOf )
import System.Environment( getArgs )
import System.Directory( createDirectoryIfMissing, getDirectoryContents )
import System.FilePath( dropExtension, (</>), (<.>), splitFileName )
import Codec.Picture( writePng )
{-import Debug.Trace-}
import Text.Printf


loadRender :: [String] -> IO ()
loadRender [] = putStrLn "not enough arguments"
loadRender [_] = putStrLn "not enough arguments"
loadRender (svgfilename:pngfilename:_) = do
  f <- loadSvgFile svgfilename
  case f of
     Nothing -> putStrLn "Error while loading SVG"
     Just doc -> 
        writePng pngfilename $ renderSvgDocument Nothing doc

type Html = String

testOutputFolder :: FilePath
testOutputFolder = "gen_test"

img :: FilePath -> Int -> Int -> Html
img path _w _h =
    printf "<img src=\"%s\" />" path

table :: [Html] -> [[Html]] -> Html
table headers cells =
        "<table>" ++ header ++ concat ["<tr>" ++ elems row ++ "</tr>\n" | row <- cells ] ++ "</table>"
  where elems row = concat ["<td>" ++ cell ++ "</td>\n" | cell <- row  ]
        header = "<tr>" ++ concat ["<th>" ++ h ++ "</th>" | h <- headers ] ++ "</tr>"

testFileOfPath :: FilePath -> FilePath
testFileOfPath path = testOutputFolder </> base <.> "png"
  where (_, base) = splitFileName path

generateFileInfo :: FilePath -> [Html]
generateFileInfo path = [img path 0 0, img pngRef 0 0,  img (testFileOfPath path) 0 0]
  where
    pngRef = dropExtension path <.> "png"

toHtmlDocument :: Html -> String
toHtmlDocument html =
    "<html><head><title>Test results</title></head><body>" ++ html ++ "</body></html"

analyzeFolder :: FilePath -> IO ()
analyzeFolder folder = do
  createDirectoryIfMissing True testOutputFolder
  fileList <- filter (".svg" `isSuffixOf`) <$> getDirectoryContents folder
  let all_table = table ["W3C Svg", "W3C ref PNG", "mine"] 
                . map generateFileInfo $ map (folder </>) fileList
      doc = toHtmlDocument all_table
      (_, folderBase) = splitFileName folder

  writeFile (folder </> ".." </> folderBase <.> "html") doc
  forM_ fileList $ \p -> do
    let realFilename = folder </> p
    putStrLn $ "Loading: " ++ realFilename
    svg <- loadSvgFile realFilename
    case svg of
      Nothing -> putStrLn $ "Failed to load " ++ p
      Just d -> do
        putStrLn $ "   => Rendering " ++ show (svgDocumentSize d)
        writePng (testFileOfPath p) $ renderSvgDocument Nothing d


testSuite :: IO ()
testSuite = analyzeFolder "w3csvg"

main :: IO ()
main = do
    args <- getArgs
    case args of
      "test":_ -> testSuite
      _ -> loadRender args

