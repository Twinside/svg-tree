import Control.Applicative( (<$>), many )
import Control.Monad( forM_ )
import Data.Attoparsec.Text( parseOnly )
import Data.Binary( encodeFile, decodeOrFail  )
import qualified Data.ByteString.Lazy as B
import Data.List( isSuffixOf, sort )
import qualified Data.Text as T
import System.Environment( getArgs )
import System.Directory( createDirectoryIfMissing
                       , getDirectoryContents
                       , doesFileExist
                       )
import System.FilePath( dropExtension, (</>), (<.>), splitFileName )

import Codec.Picture( writePng )

import Graphics.Text.TrueType( FontCache, buildCache )
import Graphics.Svg.CssParser
import Graphics.Svg
{-import Debug.Trace-}
import Text.Printf

loadCreateFontCache :: IO FontCache
loadCreateFontCache = do
  exist <- doesFileExist filename
  if exist then loadCache else createWrite
  where
    filename =  "fonty-texture-cache"
    loadCache = do
      putStrLn "Loading pre-existing font cache"
      bstr <- B.readFile filename
      case decodeOrFail bstr of
        Left _ -> do
          putStrLn "Failed to load cache, recreate"
          createWrite
        Right (_, _, v) -> do
          putStrLn "Done"
          return v
      
    createWrite = do
      putStrLn "Building font cache..."
      cache <- buildCache
      putStrLn "Saving font cache..."
      encodeFile filename cache
      putStrLn "Done"
      return cache

loadRender :: [String] -> IO ()
loadRender [] = putStrLn "not enough arguments"
loadRender [_] = putStrLn "not enough arguments"
loadRender (svgfilename:pngfilename:_) = do
  f <- loadSvgFile svgfilename
  case f of
     Nothing -> putStrLn "Error while loading SVG"
     Just doc -> do
        cache <- loadCreateFontCache
        (finalImage, _) <- renderSvgDocument cache Nothing doc
        writePng pngfilename finalImage

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

analyzeFolder :: FontCache -> FilePath -> IO ()
analyzeFolder cache folder = do
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
        (finalImage, _) <- renderSvgDocument cache Nothing d
        writePng (testFileOfPath p) finalImage


testSuite :: IO ()
testSuite = do
    cache <- loadCreateFontCache
    analyzeFolder cache "w3csvg"
    analyzeFolder cache "test"

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
  print .  parseOnly styleString $ T.pack "/* rule 10 */ stroke-dasharray:300,100"
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

