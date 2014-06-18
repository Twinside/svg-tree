import Control.Applicative( (<$>), many )
import Control.Monad( forM_ )
import Graphics.Svg
import Data.List( isSuffixOf, sort )
import System.Environment( getArgs )
import System.Directory( createDirectoryIfMissing, getDirectoryContents )
import Data.Attoparsec.Text( parseOnly )
import System.FilePath( dropExtension, (</>), (<.>), splitFileName )
import Codec.Picture( writePng )
import Graphics.Svg.CssParser
import qualified Data.Text as T
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
    printf "<img src=\"%s\" alt=\"%s\" />" path path

table :: [Html] -> [[Html]] -> Html
table headers cells =
        "<table>" ++ header ++ concat ["<tr>" ++ elems row ++ "</tr>\n" | row <- cells ] ++ "</table>"
  where elems row = concat ["<td>" ++ cell ++ "</td>\n" | cell <- row  ]
        header = "<tr>" ++ concat ["<th>" ++ h ++ "</th>" | h <- headers ] ++ "</tr>"

testFileOfPath :: FilePath -> FilePath
testFileOfPath path = testOutputFolder </> base <.> "png"
  where (_, base) = splitFileName path

text :: String -> Html
text txt = txt ++ "<br/>"

generateFileInfo :: FilePath -> [Html]
generateFileInfo path = [text path, img path 0 0, img pngRef 0 0,  img (testFileOfPath path) 0 0]
  where
    pngRef = dropExtension path <.> "png"

toHtmlDocument :: Html -> String
toHtmlDocument html =
    "<html><head><title>Test results</title></head><body>" ++ html ++ "</body></html"

analyzeFolder :: FilePath -> IO ()
analyzeFolder folder = do
  createDirectoryIfMissing True testOutputFolder
  fileList <- sort . filter (".svg" `isSuffixOf`) <$> getDirectoryContents folder
  let all_table = table ["name", "W3C Svg", "W3C ref PNG", "mine"] 
                . map generateFileInfo $ map (folder </>) fileList
      doc = toHtmlDocument all_table
      (_, folderBase) = splitFileName folder

  print fileList

  writeFile (folder </> ".." </> folderBase <.> "html") doc
  forM_ fileList $ \p -> do
    let realFilename = folder </> p
    putStrLn $ "Loading: " ++ realFilename
    svg <- loadSvgFile realFilename
    {-print svg-}
    case svg of
      Nothing -> putStrLn $ "Failed to load " ++ p
      Just d -> do
        putStrLn $ "   => Rendering " ++ show (svgDocumentSize d)
        writePng (testFileOfPath p) $ renderSvgDocument Nothing d


testSuite :: IO ()
testSuite = do
    analyzeFolder "w3csvg"
    analyzeFolder "test"

baseTest :: String
baseTest = 
  "      /* rule 1 */ #MyUse { fill: blue }" ++
  "      /* rule 2 */ #MyPath { stroke: red }" ++
  "      /* rule 3 */ use { fill-opacity: .5 }" ++
  "      /* rule 4 */ path { stroke-opacity: .5 }" ++
  "      /* rule 5 */ .MyUseClass { stroke-linecap: round }" ++
  "      /* rule 6 */ .MyPathClass { stroke-linejoin: bevel }" ++
  "      /* rule 7 */ use > path { shape-rendering: optimizeQuality }" ++
  "      /* rule 8 */ g > path { visibility: hidden }" ++
  "/* Meuh rule */ g.cata#pon .flou#pa #po {}" ++
  "g use, use g { cataran: rgb(12, 13, 15); grameu: 12; prout: #333 }"

cssParseTest :: String -> IO ()
cssParseTest css = do
  case parseOnly (many ruleSet) (T.pack css) of
    Left err -> putStrLn $ "Fail to parse " ++ err
    Right r -> mapM_ print r

main :: IO ()
main = do
    args <- getArgs
    case args of
      "test":_ -> testSuite
      "csstest":_ -> cssParseTest baseTest
      _ -> loadRender args

