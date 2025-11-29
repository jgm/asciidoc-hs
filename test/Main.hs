{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TEL
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, takeExtension)
import qualified Data.ByteString.Char8 as B
import AsciiDoc
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
  asciidoctorTests <- goldenTests "asciidoctor"
  featureTests <- goldenTests "feature"
  defaultMain $ testGroup "Tests"
    [ testGroup "Asciidoctor" asciidoctorTests
    , testGroup "Feature" featureTests
    , testGroup "Generic"
       [ foldInlineTest
       , foldBlockTest
       , mapInlineTest
       , mapBlockTest
       ]
    ]

goldenTests :: FilePath -> IO [TestTree]
goldenTests fp = do
  files <- findTestFiles ("test" </> fp)
  pure $ map (\(category, fs) ->
          testGroup category (map toGoldenTest fs)) files

-- Find all .test files in a directory
findTestFiles :: FilePath -> IO [(FilePath, [FilePath])]
findTestFiles baseDir = do
  categories <- listDirectory baseDir
  let go f = do
        xs <- map ((baseDir </> f) </>) <$> listDirectory (baseDir </> f)
        return (f, sort $ filter ((== ".test") . takeExtension) xs)
  mapM go categories

toGoldenTest :: FilePath -> TestTree
toGoldenTest fp =
  goldenVsStringDiff (takeBaseName fp) diff fp getTested
 where
  diff ref new = ["diff", "-u", ref, new]

  constructGoldenTest :: (T.Text, T.Text) -> IO BL.ByteString
  constructGoldenTest (inText, outText) =
    return $ TEL.encodeUtf8 $ TL.fromStrict $
      inText <> ">>>" <>
      "\n" <> ensureFinalNewline outText

  getTested = do
    (inText,_) <- readGoldenTest fp
    result <- convert inText
    constructGoldenTest (inText, result)

  raiseError path pos msg =
    error $ "Parse error at " <> show path <>
             " position " <> show pos <> ": " <> msg

  convert inText =
    T.pack . ppShow <$> parseDocument TIO.readFile raiseError fp inText

  readGoldenTest :: FilePath -> IO (T.Text, T.Text)
  readGoldenTest fp' = do
    lns <- B.lines <$> B.readFile fp'
    case break (B.isPrefixOf ">>>") lns of
      (inlines,outlines) -> return (TE.decodeUtf8 (B.unlines inlines),
                                    TE.decodeUtf8 (B.unlines $ drop 1outlines))


ensureFinalNewline :: T.Text -> T.Text
ensureFinalNewline xs = case T.unsnoc xs of
  Nothing        -> xs
  Just (_, '\n') -> xs
  _              -> xs <> "\n"


testDoc :: Document
testDoc = Document
  { docMeta = Meta [] Nothing [] Nothing mempty
  , docBlocks =
      [ Block mempty Nothing
          (List
             (OrderedList (Level 1) Nothing)
             [ ListItem Nothing
                 [ Block mempty Nothing
                     (Paragraph
                        [ Inline mempty (Str "nested ")
                        , Inline mempty
                            (Bold
                               [ Inline mempty (Str "inline ")
                               , Inline mempty (Italic [ Inline mempty (Str "and block") ])
                               ])
                        ])
                 , Block mempty Nothing
                     (List
                        (OrderedList (Level 2) Nothing)
                        [ ListItem Nothing
                            [ Block mempty (Just (BlockTitle [Inline mempty (Str "The title")]))
                                (Paragraph
                                   [ Inline mempty (Bold [ Inline mempty (Str "contents") ]) ])
                            ]
                        ])
                 ]
             ])
      ]
  }

foldInlineTest :: TestTree
foldInlineTest = testCase "foldInline" $ do
  foldInlines (\case
                  (Inline _ (Str s)) -> s
                  _ -> "") testDoc
    @?= "nested inline and blockThe titlecontents"

foldBlockTest :: TestTree
foldBlockTest = testCase "foldBlock" $ do
  foldBlocks (\case
                  Block _ (Just t) _ -> [t]
                  _ -> []) testDoc
    @?= [BlockTitle [Inline mempty (Str "The title")]]

mapInlineTest :: TestTree
mapInlineTest = testCase "mapInlines" $ do
  d <- mapInlines
              (\case
                 Inline _ (Str _) -> pure $ Inline mempty (Str "X")
                 x -> pure x) testDoc
  d @?= Document
      { docMeta = Meta [] Nothing [] Nothing mempty
      , docBlocks =
          [ Block mempty Nothing
              (List
                 (OrderedList (Level 1) Nothing)
                 [ ListItem Nothing
                     [ Block mempty Nothing
                         (Paragraph
                            [ Inline mempty (Str "X")
                            , Inline mempty
                                (Bold
                                   [ Inline mempty (Str "X")
                                   , Inline mempty (Italic [ Inline mempty (Str "X") ])
                                   ])
                            ])
                     , Block mempty Nothing
                         (List
                            (OrderedList (Level 2) Nothing)
                            [ ListItem Nothing
                                [ Block mempty (Just (BlockTitle [Inline mempty (Str "X")]))
                                    (Paragraph
                                       [ Inline mempty (Bold [ Inline mempty (Str "X") ]) ])
                                ]
                            ])
                     ]
                 ])
          ]
      }


mapBlockTest :: TestTree
mapBlockTest = testCase "mapBlocks" $ do
  d <- mapBlocks
            (\case
               Block attr mbtitle (Paragraph _) ->
                 pure $ Block attr mbtitle ThematicBreak
               x -> pure x) testDoc
  d @?= Document
      { docMeta = Meta [] Nothing [] Nothing mempty
      , docBlocks =
          [ Block mempty Nothing
              (List
                 (OrderedList (Level 1) Nothing)
                 [ ListItem Nothing
                     [ Block mempty Nothing ThematicBreak
                     , Block mempty Nothing
                         (List
                            (OrderedList (Level 2) Nothing)
                            [ ListItem Nothing
                                [ Block mempty (Just (BlockTitle [Inline mempty (Str "The title")]))
                                    ThematicBreak
                                ]
                            ])
                     ]
                 ])
          ]
      }
