{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import Test.Tasty.Bench
import qualified Data.Text as T
import Data.Text (Text)
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))
import AsciiDoc

main :: IO ()
main = defaultMain
  [ bgroup "prose"
      [ bench (show n <> "KB") $ nf parseSize (prose n)
      | n <- [32, 64, 128 :: Int]
      ]
  , bgroup "longword"
      [ bench (show n) $ nf parseSize (longword n)
      | n <- [1000, 2000, 4000 :: Int]
      ]
  , bgroup "formatting"
      [ bench "32KB" $ nf parseSize (formatting 32) ]
  , bgroup "table"
      [ bench (show n <> "rows") $ nf parseSize (table n)
      | n <- [250, 500, 1000 :: Int]
      ]
  ]

-- Parse a document and force its interesting parts, returning a size.
parseSize :: Text -> Int
parseSize t = docSize $ runIdentity $
  parseDocument (const (Identity "")) raiseError "bench.adoc" t
 where
  raiseError fp pos msg =
    error $ fp <> "@" <> show pos <> ": " <> msg

docSize :: Document -> Int
docSize d = getSum (foldBlocks (const (Sum 1)) d) +
            getSum (foldInlines inlineSize d)
 where
  inlineSize (Inline _ (Str s)) = Sum (T.length s)
  inlineSize _ = Sum 1

-- n KB of plain prose paragraphs.
prose :: Int -> Text
prose n = T.replicate (n * 16) paragraph
 where
  paragraph = T.replicate 7 sentence <> "\n\n"  -- ~64 bytes/sentence
  sentence = "The quick brown fox jumps over one lazy dog every morning. "

-- A single unbroken run of letters (worst case for per-letter lookahead).
longword :: Int -> Text
longword n = T.replicate n "a" <> "\n"

-- n KB of text with plenty of inline formatting.
formatting :: Int -> Text
formatting n = T.replicate (n * 16) paragraph
 where
  paragraph = T.replicate 8 chunk <> "\n\n"  -- 8 * 8 bytes
  chunk = "a *b* `c` _d_ "

-- A PSV table with n rows of five cells.
table :: Int -> Text
table n =
  "|===\n" <> T.replicate n row <> "|===\n"
 where
  row = "| alpha | beta | gamma | delta | epsilon\n"
