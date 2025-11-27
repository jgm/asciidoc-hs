{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AsciiDoc.Parse
  ( parseDocument
  ) where

import Text.HTML.TagSoup.Entity (lookupNamedEntity)
import Data.Maybe (isNothing, listToMaybe, fromMaybe)
import Data.Bifunctor (first)
import Data.Either (lefts, rights)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import Data.List (foldl', intersperse)
import qualified Data.Attoparsec.Text as A
import System.FilePath
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char (isAlphaNum, isAscii, isSpace, isLetter, isPunctuation, chr, isDigit)
import AsciiDoc.AST
import AsciiDoc.Generic
-- import Debug.Trace

-- | Parse a complete AsciiDoc document
parseDocument :: Monad m
              => (FilePath -> m Text) -- ^ Get contents of an included file
              -> (Int -> String -> m Document) -- ^ Raise an error given source pos and message
              -> FilePath -- ^ Path of file containing the text (need for include handling)
              -> Text -- ^ Text to convert
              -> m Document
parseDocument getFileContents raiseError path t =
   go (A.parse pDocument t) >>= handleIncludes path
     >>= resolveAttributeReferences . addIdentifiers
     >>= resolveCrossReferences
 where
  go (A.Fail i _ msg) = raiseError (T.length t - T.length i) msg
  go (A.Partial continue) = go (continue "")
  go (A.Done i r) | T.all isSpace i = pure r
                | otherwise = raiseError (T.length t - T.length i)
                               ("unexpected: " ++ T.unpack (T.take 20 i))

  toAnchorMap d = foldBlocks blockAnchor d <> foldInlines inlineAnchor d

  blockAnchor (Block (Attr _ kvs) _ (Section _ ils _))
    | Just ident <- M.lookup "id" kvs = M.singleton (T.pack path <> "#" <> ident) ils
  blockAnchor _ = mempty

  inlineAnchor (Inline _ (InlineAnchor ident ils))
    = M.singleton (T.pack path <> "#" <> ident) ils
  inlineAnchor (Inline _ (BibliographyAnchor ident ils))
    = M.singleton (T.pack path <> "#" <> ident) ils
  inlineAnchor _ = mempty

  resolveCrossReferences d = mapInlines (resolveCrossReference (toAnchorMap d)) d
  resolveCrossReference anchorMap (Inline attr (CrossReference ident Nothing))
    | T.take 1 ident == "#"
    , Just ils <- M.lookup ident anchorMap
      = pure $ Inline attr (CrossReference ident (Just ils))
    | Just ils <- M.lookup (T.pack path <> "#" <> ident) anchorMap
      = pure $ Inline attr (CrossReference ident (Just ils))
  resolveCrossReference _ x = pure x

  resolveAttributeReferences doc =
    mapInlines (goAttref (docAttributes (docMeta doc))) doc

  goAttref atts il@(Inline attr (AttributeReference (AttributeName at))) =
     case M.lookup at atts of
       Nothing -> return il
       Just x -> return $ Inline attr (Str x)
  goAttref _ il = return il

  handleIncludes parentPath = mapBlocks (handleIncludeBlock parentPath)

  handleIncludeBlock parentPath (Block attr mbtitle (Include fp Nothing)) = do
    let fp' = resolvePath parentPath fp
    (do contents <- getFileContents fp'
        Block attr mbtitle . Include fp' . Just . docBlocks <$>
          go (A.parse pDocument contents))
      >>= mapBlocks (handleIncludeBlock fp')
  handleIncludeBlock parentPath (Block attr mbtitle
                                  (IncludeListing mblang fp Nothing)) = do
    let fp' = resolvePath parentPath fp
    (do contents <- getFileContents fp'
        pure $ Block attr mbtitle $ IncludeListing mblang fp'
             $ Just (map (\ln -> SourceLine ln []) (T.lines contents)))
      >>= mapBlocks (handleIncludeBlock fp')
  handleIncludeBlock _ x = pure x

  resolvePath parentPath fp =
    if isRelative fp
       then takeDirectory parentPath </> fp
       else fp

type P = A.Parser

data BlockContext =
    SectionContext Int
  | ListContext Char Int
  | DelimitedContext Char Int Bool -- True if hardbreaks
  deriving (Show, Eq)

char :: Char -> P ()
char c = void $ A.char c

pDocument :: P Document
pDocument = do
  meta <- pDocumentHeader
  let minSectionLevel = case M.lookup "doctype" (docAttributes meta) of
                          Just "book" -> 0
                          _ -> 1
  bs <- many (pBlock [SectionContext (minSectionLevel - 1)])
  pure $ Document { docMeta = meta , docBlocks = bs }

-- authorAttributes :: [Author] -> M.Map Text Text
-- authorAttributes as = mempty -- TODO

pDocumentHeader :: P Meta
pDocumentHeader = do
  let handleAttr m (Left k) = M.delete k m
      handleAttr m (Right (k,v)) = M.insert k v m
  let defaultDocAttrs = M.insert "sectids" "" $ mempty
  skipBlankLines []
  topattrs <- foldl' handleAttr defaultDocAttrs <$> many pDocAttribute
  skipBlankLines []
  (title, titleAttr) <- A.option ([], Nothing) $ do
    (_,titleAttr) <- pTitlesAndAttributes
    title <- pDocumentTitle
    pure (title, case titleAttr of
                   Attr [] kv | M.null kv -> Nothing
                   _ -> Just titleAttr)
  authors <- if null title
                then pure []
                else A.option [] pDocumentAuthors
  revision <- if null title
                 then pure Nothing
                 else optional pDocumentRevision
  attrs <- foldl' handleAttr topattrs <$> many pDocAttribute
  -- TODO add authors from attributes
  -- = The Intrepid Chronicles
  -- :author: Kismet R. Lee
  -- :email: kismet@asciidoctor.org
  pure $ Meta{ docTitle = title
             , docTitleAttributes = titleAttr
             , docAuthors = authors
             , docRevision = revision
             , docAttributes = attrs }

pDocumentTitle :: P [Inline]
pDocumentTitle = do
  (char '=' <|> char '#') <* some (A.char ' ')
  parseInlines <$> pLine

pDocumentAuthors :: P [Author]
pDocumentAuthors = do
  mbc <- A.peekChar
  case mbc of
    Just c | isSpace c || c == ':' -> mzero
    _ -> parseAuthors <$> pLine

parseAuthors :: Text -> [Author]
parseAuthors =
  map (parseAuthor . T.strip) . T.split (== ';')

parseAuthor :: Text -> Author
parseAuthor t =
  Author { authorName = T.strip name
         , authorEmail = email }
 where
  (name, rest) = T.break (== '<') t
  email = case T.uncons rest of
            Just ('<', rest') -> Just $ T.takeWhile (/='>') rest'
            _ -> Nothing

pDocumentRevision :: P Revision
pDocumentRevision = do
  vprefix <- A.option False (True <$ char 'v')
  version <- A.takeWhile1 (\c -> not (A.isEndOfLine c) && c /= ',')
  date <- optional (T.strip <$> (char ',' *> A.space
               *> A.takeWhile1 (\c -> not (A.isEndOfLine c) && c /= ':')))
  remark <- optional
            (T.strip <$> (char ':' *>  A.space *> A.takeWhile (not . A.isEndOfLine)))
  A.endOfLine
  when (isNothing date && isNothing remark) $ guard vprefix
  pure  Revision { revVersion = version
                 , revDate = date
                 , revRemark = remark
                 }

pLine :: P Text
pLine = A.takeWhile (not . A.isEndOfLine) <* (A.endOfLine <|> A.endOfInput)


-- Left key unsets key
-- Right (key, val) sets key
pDocAttribute :: P (Either Text (Text, Text))
pDocAttribute = do
  char ':'
  unset <- A.option False $ True <$ char '!'
  k <- pDocAttributeName
  char ':'
  v <- pLineWithEscapes
  pure $ if unset
            then Left k
            else Right (k,v)

pDocAttributeName :: P Text
pDocAttributeName = do
  c <- A.satisfy (\d -> isAscii d && (isAlphaNum d || d == '_'))
  cs <- many $
          A.satisfy (\d -> isAscii d && (isAlphaNum d || d == '_' || d == '-'))
  pure $ T.pack (c:cs)

pLineWithEscapes :: P Text
pLineWithEscapes = do
  _ <- A.takeWhile (== ' ')
  t <- A.takeWhile isLineEndChar
  A.endOfLine
  case T.stripSuffix "\\" t of
    Nothing -> pure t
    Just t' -> do
      case T.stripSuffix " +" t' of
        Nothing -> (t' <>) <$> pLineWithEscapes
        Just t'' -> ((t'' <> "\n") <>) <$> pLineWithEscapes

isLineEndChar :: Char -> Bool
isLineEndChar '\r' = False
isLineEndChar '\n' = False
isLineEndChar _ = True

skipBlankLines :: [BlockContext] -> P ()
skipBlankLines blockContexts =
  case blockContexts of
    ListContext{} : _ -> void $ many $ char '+' *> pBlankLine
    _ -> void $ many pBlankLine

pBlankLine :: P ()
pBlankLine = A.takeWhile (\c -> c == ' ' || c == '\t') *> (pLineComment <|> A.endOfLine)

parseBlocks :: Text -> P [Block]
parseBlocks t =
  either fail pure $ A.parseOnly (many (pBlock [])) (T.strip t <> "\n")

parseParagraphs :: Text -> P [Block]
parseParagraphs t =
  either fail pure $ A.parseOnly (many pParagraph) (T.strip t <> "\n")
 where
  pParagraph = do
    skipBlankLines []
    (mbtitle, attr@(Attr _ kvs)) <- pTitlesAndAttributes
    let hardbreaks = M.lookup "options" kvs == Just "hardbreaks"
    A.skipMany (pCommentBlock attr)
    Block attr mbtitle <$> pPara [] hardbreaks

parseInlines :: Text -> [Inline]
parseInlines t = either (const mempty) id $ A.parseOnly pInlines (T.strip t)

pBlock :: [BlockContext] -> P Block
pBlock blockContexts = do
  skipBlankLines blockContexts
  (mbtitle, attr) <- pTitlesAndAttributes
  let hardbreaks = case attr of
                     (Attr _ kvs) |
                       Just "hardbreaks" <- M.lookup "options" kvs -> True
                     _ -> case blockContexts of
                            (DelimitedContext _ _ True : _) -> True
                            _ -> False
  case blockContexts of
    ListContext{} : _ -> A.skipWhile (== ' ')
    _ -> pure ()
  A.skipMany (pCommentBlock attr)
  pBlockMacro mbtitle attr
    <|> pDiscreteHeading mbtitle attr
    <|> pExampleBlock mbtitle attr
    <|> pSidebar mbtitle attr
    <|> pLiteralBlock mbtitle attr
    <|> pListing mbtitle attr
    <|> pFenced mbtitle attr
    <|> pVerse mbtitle attr
    <|> pQuoteBlock mbtitle attr
    <|> pPassBlock mbtitle attr
    <|> pOpenBlock mbtitle attr
    <|> pTable mbtitle attr
    <|> Block attr mbtitle <$>
          A.choice
            [ pSection blockContexts
            , pThematicBreak
            , pPageBreak
            , pList blockContexts
            , pDefinitionList blockContexts
            , pIndentedLiteral
            , pPara blockContexts hardbreaks
            ]

pTableBorder :: P TableSyntax
pTableBorder = do
  syntax <- (PSV <$ char '|') <|> (DSV <$ char ':') <|> (CSV <$ char ',')
  void $ A.string "==="
  A.skipWhile (=='=')
  pBlankLine
  A.skipMany pBlankLine
  pure syntax

pTable :: Maybe BlockTitle -> Attr -> P Block
pTable mbtitle (Attr ps kvs) = do
  syntax' <- pTableBorder
  mbcolspecs <- maybe (pure Nothing) (fmap Just . parseColspecs)
                  (M.lookup "cols" kvs)
  let options = maybe [] T.words $ M.lookup "options" kvs
  let syntax = case M.lookup "format" kvs of
                 Just "psv" -> PSV
                 Just "csv" -> CSV
                 Just "dsv" -> DSV
                 Just "tsv" -> TSV
                 _ -> syntax'
  let mbsep = case M.lookup "separator" kvs of
                 Just sep ->
                   case T.uncons sep of
                     Just (c,_) -> Just c
                     _ -> Nothing
                 _ -> Nothing
  let tableOpts = TableOpts { tableSyntax = syntax
                            , tableSeparator = mbsep
                            , tableHeader = "header" `elem` options ||
                                "noheader" `notElem` options
                            , tableFooter = "footer" `elem` options ||
                                "nofooter" `notElem` options
                            }
  let getRows mbspecs rowspans = (([],[]) <$ pTableBorder) <|>
         do -- for this row, we modify the specs based on rowspans
            -- if there are rowspans from rows above, we need to skip some:
            let mbspecs' = case mbspecs of
                             Nothing -> Nothing
                             Just specs' -> Just [s | (s,0) <- zip specs' rowspans]
            row@(TableRow cells) <- pTableRow tableOpts mbspecs'
            let numcols = sum (map cellColspan cells)
            let specs = fromMaybe (replicate numcols defaultColumnSpec) mbspecs
            -- now, update rowspans in light of new row
            let updateRowspans [] rs = rs
                updateRowspans (c:cs) rs =
                  map (+ (cellRowspan c - 1)) (take (cellColspan c) rs)
                  ++ updateRowspans cs (drop (cellColspan c) rs)
            let rowspans' = updateRowspans cells rowspans
            (\(rows, colspecs') -> (row:rows, case rows of
                                                 [] -> specs
                                                 _ -> colspecs'))
                                     <$> getRows (Just specs) rowspans'
  (rows, colspecs') <- getRows mbcolspecs (repeat (0 :: Int))
  let attr' = Attr ps $ M.delete "format" .
                        M.delete "separator" .
                        M.delete "cols" .
                        M.delete "options" $ kvs
  let (mbHead, rest)
        | tableHeader tableOpts = (Just (take 1 rows), drop 1 rows)
        | otherwise = (Nothing, rows)
  let (mbFoot, bodyRows)
        | tableFooter tableOpts
        , not (null rest) = (Just (drop (length rest - 1) rest),
                             take (length rest - 1) rest)
        | otherwise = (Nothing, rest)
  pure $ Block attr' mbtitle $ Table colspecs' mbHead bodyRows mbFoot

parseColspecs :: T.Text -> P [ColumnSpec]
parseColspecs t =
  case A.parseOnly pColspecs t of
    Left e -> fail e
    Right cs -> pure cs

pColspecs :: P [ColumnSpec]
pColspecs = mconcat <$> A.sepBy pColspecPart pComma <* A.option () pComma

pColspecPart :: P [ColumnSpec]
pColspecPart = do
  multiplier <- A.option 1 pMultiplier
  replicate multiplier <$> pColspec

pMultiplier :: P Int
pMultiplier = A.decimal <* char '*'

pColspec :: P ColumnSpec
pColspec = ColumnSpec <$> optional pHorizAlign
                      <*> optional pVertAlign
                      <*> (pWidth <|> pure Nothing)
                      <*> (toCellStyle <$> A.satisfy (A.inClass "adehlms")
                             <|> pure Nothing)

pHorizAlign :: P HorizAlign
pHorizAlign =
  (AlignLeft <$ char '<') <|> (AlignCenter <$ char '^') <|> (AlignRight <$ char '>')

pVertAlign :: P VertAlign
pVertAlign = do
  char '.'
  (AlignTop <$ char '<') <|> (AlignMiddle <$ char '^') <|> (AlignBottom <$ char '>')

pWidth :: P (Maybe Int)
pWidth = (Just <$> (A.decimal <* A.option () (char '%'))) <|> (Nothing <$ char '~')

data TableSyntax =
    PSV
  | CSV
  | TSV
  | DSV
  deriving (Show)

data TableOpts =
  TableOpts { tableSyntax :: TableSyntax
            , tableSeparator :: Maybe Char
            , tableHeader :: Bool
            , tableFooter :: Bool
            }
  deriving (Show)

pTableRow :: TableOpts -> Maybe [ColumnSpec] -> P TableRow
pTableRow opts mbcolspecs = TableRow <$>
  case tableSyntax opts of
       PSV
         | Just colspecs <- mbcolspecs  ->
             let getCell :: [ColumnSpec] -> P [TableCell]
                 getCell [] = pure []
                 getCell colspecs' = do
                   xs <- pTableCellPSV (tableSeparator opts) True colspecs'
                   A.skipMany pBlankLine
                   (xs ++) <$> getCell (drop (sum (map cellColspan xs)) colspecs')
             in  getCell colspecs
         | otherwise -> mconcat <$>
               some (pTableCellPSV (tableSeparator opts)
                       False (repeat defaultColumnSpec))
                     <* A.skipMany pBlankLine
       CSV -> pCSVTableRow (fromMaybe ',' $ tableSeparator opts) mbcolspecs
       TSV -> pCSVTableRow (fromMaybe '\t' $ tableSeparator opts) mbcolspecs
       DSV -> pDSVTableRow (fromMaybe ':' $ tableSeparator opts) mbcolspecs

defaultColumnSpec :: ColumnSpec
defaultColumnSpec = ColumnSpec Nothing Nothing Nothing Nothing

-- Note: AsciiDoc weirdly gobbles cells for rows even across CSV
-- row boundaries. We're not going to do that.

-- allows "; escape this as ""; delim can't be escaped
pCSVTableRow :: Char -> Maybe [ColumnSpec] -> P [TableCell]
pCSVTableRow delim mbcolspecs = do
  let colspecs = fromMaybe [] mbcolspecs
  as <- A.sepBy (pCSVCell delim) (char delim)
  pBlankLine *> A.skipMany pBlankLine
  zipWithM toBasicCell as (colspecs ++ repeat defaultColumnSpec)

pCSVCell :: Char -> P T.Text
pCSVCell delim = do
  A.skipWhile (== ' ')
  mbc <- A.peekChar
  case mbc of
    Just '"'
      -> char '"' *>
          (T.pack <$>
            A.manyTill (A.satisfy (/='"') <|> ('"' <$ A.string "\"\"")) (char '"'))
    _ -> T.strip . T.replace "\"\"" "\"" <$>
           A.takeWhile1 (\c -> c /= delim && not (A.isEndOfLine c))

-- no "; escape delim with backslash
pDSVTableRow:: Char -> Maybe [ColumnSpec] -> P [TableCell]
pDSVTableRow delim mbcolspecs = do
  let colspecs = fromMaybe [] mbcolspecs
  as <- A.sepBy (pDSVCell delim) (char delim)
  pBlankLine *> A.skipMany pBlankLine
  zipWithM toBasicCell as (colspecs ++ repeat defaultColumnSpec)

pDSVCell :: Char -> P T.Text
pDSVCell delim =
  T.strip . mconcat <$>
    many (A.takeWhile1 (\c -> c /= delim && c /= '\\' && not (A.isEndOfLine c))
       <|> (char '\\' *> ((\c -> "\\" <> T.singleton c) <$> A.anyChar)))

toBasicCell :: T.Text -> ColumnSpec -> P TableCell
toBasicCell t colspec = do
  bs <- parseCellContents (fromMaybe DefaultStyle (colStyle colspec)) t
  pure TableCell
         { cellContent = bs
         , cellHorizAlign = Nothing
         , cellVertAlign = Nothing
         , cellColspan = 1
         , cellRowspan = 1
         }


pTableCellPSV :: Maybe Char -> Bool -> [ColumnSpec] -> P [TableCell]
pTableCellPSV mbsep allowNewlines colspecs = do
  let sep = fromMaybe '|' mbsep
  cellData <- pCellSep sep
  t <- T.pack <$>
         many
          (notFollowedBy (void (pCellSep sep) <|> void pTableBorder) *>
           ((char '\\' *> A.char sep)
             <|> A.satisfy (not . A.isEndOfLine)
             <|> if allowNewlines
                    then A.satisfy A.isEndOfLine
                    else A.satisfy A.isEndOfLine <* notFollowedBy (pCellSep sep)))
  let cell' = TableCell
               { cellContent = []
               , cellHorizAlign = cHorizAlign cellData
               , cellVertAlign = cVertAlign cellData
               , cellColspan = fromMaybe 1 $ cColspan cellData
               , cellRowspan = fromMaybe 1 $ cRowspan cellData
               }
  let rawcells = replicate (cDuplicate cellData) (cell', t)
  reverse . fst <$> foldM (\(cells, specs) (cell, rawtext) -> do
                        let defsty = case specs of
                                       spec:_ -> colStyle spec
                                       _ -> Nothing
                        let sty = fromMaybe DefaultStyle $ cStyle cellData <|> defsty
                        bs <- parseCellContents sty rawtext
                        pure (cell{ cellContent = bs } : cells,
                              drop (cellColspan cell) specs))
                ([],colspecs)
                rawcells


parseCellContents :: CellStyle -> T.Text -> P [Block]
parseCellContents sty t =
  case sty of
    AsciiDocStyle ->
      either (fail . show) (pure . docBlocks)
       (parseDocument (\_ -> pure mempty)
       (\pos msg -> Left $ "Parse error at position " <> show pos <> ": " <> msg)
       "table-cell"  -- TODO somehow get file path here 
        (t <> "\n"))
    DefaultStyle -> parseParagraphs t
    LiteralStyle -> pure [Block mempty Nothing $ LiteralBlock t]
    EmphasisStyle -> map (surroundPara Italic) <$> parseBlocks t
    StrongStyle -> map (surroundPara Bold) <$> parseBlocks t
    MonospaceStyle -> map (surroundPara Monospace) <$> parseBlocks t
    HeaderStyle -> parseBlocks t
 where
   surroundPara :: ([Inline] -> InlineType) -> Block -> Block
   surroundPara bt (Block attr mbtitle (Paragraph ils)) =
     Block attr mbtitle (Paragraph [Inline mempty $ bt ils])
   surroundPara _ b = b


data CellData =
  CellData
  { cDuplicate :: Int
  , cHorizAlign :: Maybe HorizAlign
  , cVertAlign :: Maybe VertAlign
  , cColspan :: Maybe Int
  , cRowspan :: Maybe Int
  , cStyle :: Maybe CellStyle }
  deriving (Show)

toCellStyle :: Char -> Maybe CellStyle
toCellStyle 'a' = Just AsciiDocStyle
toCellStyle 'd' = Just DefaultStyle
toCellStyle 'e' = Just EmphasisStyle
toCellStyle 'h' = Just HeaderStyle
toCellStyle 'l' = Just LiteralStyle
toCellStyle 'm' = Just MonospaceStyle
toCellStyle 's' = Just StrongStyle
toCellStyle _   = Nothing

-- 2+| colspan 2
-- 3.+| rowspan 3
-- 2.3+| colspan 2, rowspan 3
-- 2*| duplicate cell twice
-- 2*.3+^.>s| duplicate 2x, rowspan 3, top align, right align, s style
pCellSep :: Char -> P CellData
pCellSep sep = do
  mult <- A.option 1 pMultiplier
  (colspan, rowspan) <- A.option (Nothing, Nothing) $ do
    a <- optional A.decimal
    b <- optional $ char '.' *> A.decimal
    guard $ not (isNothing a && isNothing b)
    char '+'
    pure (a, b)
  halign <- optional pHorizAlign
  valign <- optional pVertAlign
  sty <- (toCellStyle <$> A.satisfy (A.inClass "adehlms")) <|> pure Nothing
  notFollowedBy pTableBorder <* char sep
  pure $ CellData
    { cDuplicate = mult
    , cHorizAlign = halign
    , cVertAlign = valign
    , cColspan = colspan
    , cRowspan = rowspan
    , cStyle = sty
    }

pIndentedLiteral :: P BlockType
pIndentedLiteral = do
  xs <- some pIndentedLine
  let minIndent = minimum (map fst xs)
  let xs' = map (first (\x -> x - minIndent)) xs
  let t = T.unlines $ map (\(ind, x) -> T.replicate ind " " <> x) xs'
  pure $ LiteralBlock t

pIndentedLine :: P (Int, Text)
pIndentedLine = do
  ind <- length <$> some (char ' ')
  t <- pLine
  pure (ind, t)

pPageBreak :: P BlockType
pPageBreak = PageBreak <$ (A.string "<<<" <* pBlankLine)

pThematicBreak :: P BlockType
pThematicBreak = ThematicBreak <$
  (pThematicBreakAsciidoc <|> pThematicBreakMarkdown '-' <|> pThematicBreakMarkdown '*')
 where
   pThematicBreakAsciidoc = A.string "'''" *> pBlankLine
   pThematicBreakMarkdown c = A.count 3 (char c *> many (char ' ')) *> pBlankLine

pCommentBlock :: Attr -> P ()
pCommentBlock attr = pDelimitedCommentBlock <|> pAlternateCommentBlock
 where
  pDelimitedCommentBlock = void $ pDelimitedLiteralBlock '/' 4
  pAlternateCommentBlock = do
    case attr of
      Attr ["comment"] _ ->
        (void (pDelimitedLiteralBlock '-' 2) <|>
                (void $ A.match (pPara [SectionContext (-1)] False)))
      _ -> mzero

pBlockMacro :: Maybe BlockTitle -> Attr -> P Block
pBlockMacro mbtitle attr = do
  (name, target) <- pBlockMacro'
  handleBlockMacro mbtitle attr name target

pBlockMacro' :: P (Text, Text)
pBlockMacro' = do
  name <- A.choice (map (\n -> A.string n <* A.string "::") (M.keys blockMacros))
  let targetChars = mconcat <$> some
        (A.takeWhile1 (\c -> not (isSpace c) && c /= '[' && c /= '+')
         <|>
         (char '\\' *> (T.singleton <$> A.satisfy (\c -> c == '[' || c == '+')))
         <|>
         (do Inline _ (Str t) <- pInMatched False '+' mempty Str
             pure t))
  target <- mconcat <$> many targetChars
  pure (name, target)

handleBlockMacro :: Maybe BlockTitle -> Attr -> Text -> Text -> P Block
handleBlockMacro mbtitle attr name target =
  case M.lookup name blockMacros of
    Nothing -> mzero
    Just f -> f mbtitle attr target

blockMacros :: M.Map Text (Maybe BlockTitle -> Attr -> Text -> P Block)
blockMacros = M.fromList
  [ ("image", \mbtitle attr target -> do
        (Attr ps kvs) <- pAttributes
        let (mbalt, mbw, mbh) =
              case ps of
                (x:y:z:_) -> (Just (AltText x),
                              Width <$> readDecimal y, Height <$> readDecimal z)
                [x,y] -> (Just (AltText x), Width <$> readDecimal y, Nothing)
                [x] -> (Just (AltText x), Nothing, Nothing)
                [] -> (Nothing, Nothing, Nothing)
        pure $ Block (Attr mempty kvs <> attr) mbtitle
             $ BlockImage (Target target) mbalt mbw mbh)
  , ("video", \mbtitle attr target -> do
        attr' <- pAttributes
        pure $ Block (attr' <> attr) mbtitle
             $ BlockVideo (Target target))
  , ("audio", \mbtitle attr target -> do
        attr' <- pAttributes
        pure $ Block (attr' <> attr) mbtitle
             $ BlockAudio (Target target))
  , ("toc", \mbtitle attr _target -> do
        attr' <- pAttributes
        pure $ Block (attr' <> attr) mbtitle TOC)
  , ("include", \mbtitle attr target -> do
        attr' <- pAttributes
        pure $ Block (attr' <> attr) mbtitle $ Include (T.unpack target) Nothing)
  ]

pSection :: [BlockContext] -> P BlockType
pSection blockContexts@(SectionContext sectionLevel : _) = do
  lev <- (\x -> length x - 1) <$> (some (char '=') <|> some (char '#'))
  guard (lev > sectionLevel && lev >= 0 && lev <= 5)
  char ' '
  title <- parseInlines <$> pLine
  contents <- many (pBlock (SectionContext lev : blockContexts))
  -- note: we use sectionLevel, not lev, so in improperly nested content, e.g.,
  -- == foo
  -- ==== bar
  -- ==== baz
  -- bar is a level-3 section and will contain baz!
  pure $ Section (Level (sectionLevel + 1)) title contents
pSection _ = mzero

pDiscreteHeading :: Maybe BlockTitle -> Attr -> P Block
pDiscreteHeading mbtitle attr = do
  let (Attr ps kvs) = attr
  guard $ case ps of
            ("discrete":_) -> True
            _ -> False
  lev <- (\x -> length x - 1) <$> (some (char '=') <|> some (char '#'))
  guard (lev >= 0 && lev <= 5)
  char ' '
  title <- parseInlines <$> pLine
  pure $ Block (Attr (drop 1 ps) kvs) mbtitle $ DiscreteHeading (Level lev) title

pTitlesAndAttributes :: P (Maybe BlockTitle, Attr)
pTitlesAndAttributes = do
  items <- many pTitleOrAttribute
  let title = listToMaybe $ lefts items
  let attr = mconcat $ rights items
  pure (title, attr)

pTitleOrAttribute :: P (Either BlockTitle Attr)
pTitleOrAttribute =
  ((Left <$> pTitle)
    <|> (Right <$> (pAnchor <* A.endOfLine))
    <|> (Right <$> (pAttributes <* A.endOfLine))
  ) <* A.skipMany pBlankLine

pAnchor :: P Attr
pAnchor = do
  void $ A.string "[["  -- [[anchor]] can set id
  anchor <- A.takeWhile1 (\c -> not (A.isEndOfLine c || c == ']' || isSpace c))
  void $ A.string "]]"
  pure (Attr mempty (M.singleton "id" anchor))

pTitle :: P BlockTitle
pTitle = BlockTitle . parseInlines <$>
           (do char '.'
               mbc <- A.peekChar
               guard $ case mbc of
                         Just ' ' -> False
                         Just '.' -> False
                         _ -> True
               pLineWithEscapes)

pDefinitionList :: [BlockContext] -> P BlockType
pDefinitionList blockContexts =
  DefinitionList <$> some (pDefinitionListItem blockContexts)

pDefinitionListItem :: [BlockContext] -> P ([Inline],[Block])
pDefinitionListItem blockContexts = do
  let marker = (do t <- A.takeWhile1 (== ':')
                   case blockContexts of
                       ListContext ':' n : _ -> guard (T.length t == n + 2)
                       _ -> guard (T.length t == 2))
  A.skipWhile (== ' ')
  term <- parseInlines . mconcat
             <$> A.manyTill (A.takeWhile1 (\c -> not (A.isEndOfLine c || c == ':'))
                              <|> A.takeWhile1 (==':')) marker
  A.skipWhile (== ' ')
  A.option () A.endOfLine
  A.skipWhile (== ' ')
  let newContext = case blockContexts of
                      ListContext ':' n : _ -> ListContext ':' (n + 1)
                      _ -> ListContext ':' 1
  defn <- many (pBlock (newContext:blockContexts))
  void $ many pBlankLine
  pure (term, defn)

pList :: [BlockContext] -> P BlockType
pList blockContexts = do
  (c, lev, mbStart, mbCheckboxState) <- pAnyListItemStart
  let guardContext ctx =
       case ctx of
         ListContext c' lev' -> guard $ c /= c' || lev > lev'
         _ -> pure ()
  mapM_ guardContext blockContexts
  ListItem _ bs <- pListItem (ListContext c lev : blockContexts)
  let x = ListItem mbCheckboxState bs
  xs <- many (pListItemStart c lev *> pListItem (ListContext c lev : blockContexts))
  let listType
        | c == '-'
        , Just _ <- mbCheckboxState
          = CheckList
        | c == '.' || c == '1' = OrderedList (Level lev) mbStart
        | c == '<' = CalloutList
        | otherwise = BulletList (Level lev)
  pure $ List listType (x:xs)

pAnyListItemStart :: P (Char, Int, Maybe Int, Maybe CheckboxState)
pAnyListItemStart = (do
  A.skipWhile (== ' ')
  c <- A.satisfy (\c -> c == '*' || c == '.' || c == '-' || c == '<')
  lev <- if c == '<'
            then pure 1
            else (+ 1) . T.length <$> A.takeWhile (== c)
  when (c == '<') $ do  -- callout list <1> or <.>
    void $ A.string "." <|> A.takeWhile1 isDigit
    char '>'
  char ' '
  mbCheck <- if c == '-' || c == '*'
                then optional pCheckbox
                else pure Nothing
  pure (c, lev, Nothing, mbCheck))
 <|> (do d <- A.decimal
         char '.'
         char ' '
         pure ('1', 1, Just d, Nothing))


pCheckbox :: P CheckboxState
pCheckbox = do
  A.skipWhile (==' ')
  char '['
  c <- A.char ' ' <|> A.char 'x' <|> A.char '*'
  char ']'
  char ' '
  pure $ if c == ' '
            then Unchecked
            else Checked

pListItemStart :: Char -> Int -> P ()
pListItemStart c lev = do
  A.skipWhile (== ' ')
  case c of
    '<' -> char '<' *> (A.string "." <|> A.takeWhile1 isDigit) *> char '>'
    '1' -> do guard (lev == 1)
              void (A.decimal :: P Int)
              char '.'
    _ -> void $ A.count lev (char c)
  char ' '

pListItem :: [BlockContext] -> P ListItem
pListItem blockContexts = do
  mbCheckboxState <- optional pCheckbox
  A.skipWhile (==' ')
  bs <- many (pBlock blockContexts)
  pure $ ListItem mbCheckboxState bs

pDelimitedLiteralBlock :: Char -> Int -> P [T.Text]
pDelimitedLiteralBlock c minimumNumber = do
  len <- length <$> some (char c) <* pBlankLine
  guard $ len >= minimumNumber
  let endFence = A.count len (char c) *> pBlankLine
  A.manyTill pLine endFence

pDelimitedBlock :: Char -> Int -> Bool -> P [Block]
pDelimitedBlock c minimumNumber hardbreaks = do
  len <- length <$> some (char c) <* pBlankLine
  guard $ len >= minimumNumber
  let endFence = A.count len (char c) *> pBlankLine
  A.manyTill (pBlock [DelimitedContext c minimumNumber hardbreaks]) endFence

pPassBlock :: Maybe BlockTitle -> Attr -> P Block
pPassBlock mbtitle attr = do
  t <- T.unlines <$> pDelimitedLiteralBlock '+' 4
  case attr of
    Attr ("stem":ps) kvs ->
      pure $ Block (Attr ps kvs) mbtitle $ MathBlock Nothing t
    Attr ("asciimath":ps) kvs ->
      pure $ Block (Attr ps kvs) mbtitle $ MathBlock (Just AsciiMath) t
    Attr ("latexmath":ps) kvs ->
      pure $ Block (Attr ps kvs) mbtitle $ MathBlock (Just LaTeXMath) t
    _ -> pure $ Block attr mbtitle $ PassthroughBlock t

pLiteralBlock :: Maybe BlockTitle -> Attr -> P Block
pLiteralBlock mbtitle attr =
  (Block attr mbtitle . LiteralBlock . T.unlines <$> pDelimitedLiteralBlock '.' 4)
  <|>
  case attr of
    Attr ("literal":ps) kvs -> do
      t <- T.unlines <$> A.manyTill pLine (pBlankLine <|> A.endOfInput)
      pure $ Block (Attr ps kvs) mbtitle $ LiteralBlock t
    _ -> mzero

pFenced :: Maybe BlockTitle -> Attr -> P Block
pFenced mbtitle attr = do
  ticks <- A.takeWhile1 (== '`')
  guard $ T.length ticks >= 3
  lang' <- pLine
  let mblang = case T.strip lang' of
                 "" -> Nothing
                 l -> Just (Language l)
  lns <- toSourceLines <$> A.manyTill pLine (A.string ticks)
  pure $ Block attr mbtitle $ Listing mblang lns

pListing :: Maybe BlockTitle -> Attr -> P Block
pListing mbtitle attr = (do
  let (mbLang, attr') =
        case attr of
          Attr (_:lang:ps) kvs -> (Just (Language lang), Attr ps kvs)
          Attr ["source"] kvs -> (Nothing, Attr [] kvs)
          _ -> (Nothing, attr)
  lns <- toSourceLines <$> pDelimitedLiteralBlock '-' 4
  pure $ Block attr' mbtitle $
    case lns of
      [SourceLine x []] | "include::" `T.isPrefixOf` x
          , Right ("include", target) <- A.parseOnly pBlockMacro' x
          -> IncludeListing mbLang (T.unpack target) Nothing
      _ -> Listing mbLang lns)
 <|>
  (case attr of
    Attr ("listing":ps) kvs -> do
      lns <- toSourceLines <$> A.manyTill pLine (pBlankLine <|> A.endOfInput)
      pure $ Block (Attr ps kvs) mbtitle $ Listing Nothing lns
    Attr ("source":lang:ps) kvs -> do
      lns <- toSourceLines <$> A.manyTill pLine (pBlankLine <|> A.endOfInput)
      pure $ Block (Attr ps kvs) mbtitle
           $ Listing (Just (Language lang)) lns
    Attr ["source"] kvs -> do
      lns <- toSourceLines <$> A.manyTill pLine (pBlankLine <|> A.endOfInput)
      pure $ Block (Attr [] kvs) mbtitle $ Listing Nothing lns
    _ -> mzero)

-- parse out callouts
toSourceLines :: [T.Text] -> [SourceLine]
toSourceLines = go 1
 where
   go _ [] = []
   go nextnum (t:ts) =
     let (t', callouts) = getCallouts [] t
         (nextnum'', callouts') =
                    foldl' (\(nextnum', cs) c ->
                               case c of
                                 Nothing -> (nextnum' + 1, Callout nextnum' : cs)
                                 Just i -> (i + 1, Callout i : cs))
                       (nextnum, []) callouts
     in SourceLine t' (reverse callouts') : go nextnum'' ts
   getCallouts callouts t =
    case T.breakOnAll "<" t of
      [] -> (t, callouts)
      xs@(_:_) ->
        let (t', rest) = last xs
            (ds, rest') = T.span (\c -> isDigit c || c == '.') (T.drop 1 rest)
         in if T.strip rest' == ">" && (T.all isDigit ds || ds == ".")
               then
                 if ds == "."
                    then getCallouts (Nothing : callouts) (T.stripEnd t')
                    else case readDecimal ds of
                           Just num -> getCallouts (Just num : callouts) (T.stripEnd t')
                           Nothing -> (t, callouts)
               else (t, callouts)

pExampleBlock :: Maybe BlockTitle -> Attr -> P Block
pExampleBlock mbtitle attr = do
  bs <- pDelimitedBlock '=' 4 False
  pure $ case attr of
    Attr (p:ps) kvs |
      Just adm <- parseAdmonitionType p ->
        Block (Attr ps kvs) mbtitle $ Admonition adm bs
    _ -> Block attr mbtitle $ ExampleBlock bs

pSidebar :: Maybe BlockTitle -> Attr -> P Block
pSidebar mbtitle attr =
  Block attr mbtitle . Sidebar <$> pDelimitedBlock '*' 4 False

pVerse :: Maybe BlockTitle -> Attr -> P Block
pVerse mbtitle (Attr ("verse":xs) kvs) = do
  let attribution = T.intercalate ", " xs
  let mbAttribution = if T.null attribution
                         then Nothing
                         else Just (Attribution attribution)
  bs <- pDelimitedBlock '-' 2 True
       <|> pDelimitedBlock '_' 4 True
       <|> ((:[]) . Block mempty Nothing <$> pPara [] True)
  pure $ Block (Attr [] kvs) mbtitle $ Verse mbAttribution bs
pVerse _ _ = mzero

pQuoteBlock :: Maybe BlockTitle -> Attr -> P Block
pQuoteBlock mbtitle (Attr ("quote":xs) kvs) = do
  let attribution = T.intercalate ", " xs
  let mbAttribution = if T.null attribution
                         then Nothing
                         else Just (Attribution attribution)
  bs <- pDelimitedBlock '_' 4 True
       <|> pDelimitedBlock '-' 2 True
       <|> ((:[]) . Block mempty Nothing <$> pPara [] False)
  pure $ Block (Attr [] kvs) mbtitle $ QuoteBlock mbAttribution bs
pQuoteBlock _ _ = mzero

pOpenBlock :: Maybe BlockTitle -> Attr -> P Block
pOpenBlock mbtitle attr = Block attr mbtitle <$>
  ((OpenBlock <$> pDelimitedBlock '-' 2 False)
   <|>
  (QuoteBlock Nothing <$>
     (pDelimitedBlock '-' 2 False
      <|> pDelimitedBlock '_' 4 False)))

parseAdmonitionType :: T.Text -> Maybe AdmonitionType
parseAdmonitionType t =
  case t of
    "NOTE" -> Just Note
    "TIP" -> Just Tip
    "IMPORTANT" -> Just Important
    "CAUTION" -> Just Caution
    "WARNING" -> Just Warning
    _ -> Nothing

-- parameter is true if hardbreaks option is set
pPara :: [BlockContext] -> Bool -> P BlockType
pPara blockContexts hardbreaks = do
  t' <- pNormalLine blockContexts
  case blockContexts of
    SectionContext{} : _ | not (T.null t') -> do
      case T.head t' of
        c | c == '=' || c == '#' -> do
          let eqs = T.length $ T.takeWhile (==c) t'
          let after = T.take 1 $ T.dropWhile (==c) t'
          guard $ eqs < 1 || eqs > 6 || after /= " " -- section heading
        _ -> pure ()
    _ -> pure ()
  let (a,b) = T.break (== ':') t'
  let (t, mbAdmonition)
        = if ": " `T.isPrefixOf` b
          then
            let newt = T.drop 2 b
            in  case parseAdmonitionType a of
                 Just adm -> (newt, Just adm)
                 Nothing -> (t', Nothing)
          else (t', Nothing)
  ts <- many (pNormalLine blockContexts)
  let ils = (if hardbreaks
                then newlinesToHardbreaks
                else id) $ parseInlines $ T.unlines (t:ts)
  pure $ case mbAdmonition of
           Nothing -> Paragraph ils
           Just admonType -> Admonition admonType
                  [Block mempty Nothing (Paragraph ils)]

newlinesToHardbreaks :: [Inline] -> [Inline]
newlinesToHardbreaks [] = []
newlinesToHardbreaks (Inline attr (Str t) : xs) | T.any (=='\n') t =
  intersperse (Inline attr HardBreak)
    (map (Inline attr . Str) (T.lines t)) ++ newlinesToHardbreaks xs
newlinesToHardbreaks (x : xs) = x : newlinesToHardbreaks xs

pNormalLine :: [BlockContext] -> P Text
pNormalLine blockContexts = do
  t <- pLine
  guard $ not $ T.all (\c -> c == ' ' || c == '\t') t
  guard $ T.take 1 t /= "[" ||
          case A.parseOnly (pAttributes *> A.skipWhile isSpace *> A.endOfInput) t of
                Left _ -> True
                _ -> False
  let t' = T.stripEnd t
  let delims = [(c, num) | DelimitedContext c num _ <- blockContexts]
  mapM_ (\(c, num) -> guard (not (T.all (== c) t' && T.length t' == num)))
        delims
  case blockContexts of
    DelimitedContext c num _ : _ ->
      guard $ t' /= T.pack (replicate num c)
    ListContext{} : _ -> do
      guard $ t' /= "+"
      guard $ not $ "::" `T.isInfixOf` t'
      guard $ case A.parseOnly pAnyListItemStart (T.strip t) of
                Left _ -> True
                _ -> False
    _ -> pure ()
  pure t

pInlines :: P [Inline]
pInlines = pInlines' []

pComma :: P ()
pComma = char ',' <* A.skipWhile isSpace

pFormattedTextAttributes :: P Attr
pFormattedTextAttributes = do
  char '['
  as <- pShorthandAttributes
  ps <- A.option []
         (do unless (as == mempty) pComma
             A.sepBy1 pAttributeValue pComma <* A.option () pComma)
  char ']'
  if as == mempty
     then
       case ps of
         [] -> pure mempty
         (x:_) -> pure $ Attr [] (M.fromList [("role",x)])
     else pure as

pAttributes :: P Attr
pAttributes = do
  char '['
  as <- pShorthandAttributes
  bs <- A.option []
         (do unless (as == mempty) pComma
             A.sepBy pAttribute pComma <* A.option () pComma)
  char ']'
  let positional = lefts bs
  let kvs = rights bs
  pure $ as <> Attr positional (M.fromList kvs)

pAttribute :: P (Either Text (Text,Text))
pAttribute = pKeyValue <|> pPositional
 where
   pKeyValue = do
     k <- A.takeWhile1 (\c -> c /= ',' && c /= ']' && c /= '=')
     char '=' *> (Right . (k,) <$> pAttributeValue)
   pPositional = do
     v <- pAttributeValue
     mbc <- A.peekChar
     case mbc of
       Just ',' -> pure ()
       _ -> guard $ not $ T.null v
     pure $ Left v

pAttributeValue :: P Text
pAttributeValue = pQuotedAttr <|> pBareAttributeValue
 where
   pBareAttributeValue = T.strip <$> A.takeWhile (\c -> c /= ',' && c /= ']')
   pQuotedAttr = do
     char '"'
     result <- many (A.satisfy (/='"') <|> (char '\\' *> A.satisfy (/='"')))
     char '"'
     pure $ T.pack result

pInlines' :: [Char] -> P [Inline]
pInlines' cs =
  (do il' <- pInline cs
      let il = case il' of
                 Inline (Attr ps kvs) (Span ils)
                   | Nothing <- M.lookup "role" kvs
                   -> Inline (Attr ps kvs) (Highlight ils)
                 _ -> il'
      addStr . (il:) <$> pInlines' [])
  <|> (do c <- A.anyChar
          pInlines' (c:cs))
  <|> (addStr [] <$ A.endOfInput)
 where
  addStr = case cs of
              [] -> id
              _  -> (Inline mempty (Str (T.pack (replaceChars $ reverse cs))):)

replaceChars :: [Char] -> [Char]
replaceChars [] = []
replaceChars ('(':'C':')':cs) = '\169':replaceChars cs
replaceChars ('(':'R':')':cs) = '\174':replaceChars cs
replaceChars ('(':'T':'M':')':cs) = '\8482':replaceChars cs
replaceChars (x:'-':'-':y:cs)
  | x == ' ', y == ' ' = '\8201':'\8212':'\8201':replaceChars cs
  | isAlphaNum x, isAlphaNum y = x:'\8212':'\8203':replaceChars (y:cs)
  | otherwise = x:'-':'-':replaceChars (y:cs)
replaceChars ('.':'.':'.':cs) = '\8230':replaceChars cs
replaceChars ('-':'>':cs) = '\8594':replaceChars cs
replaceChars ('=':'>':cs) = '\8658':replaceChars cs
replaceChars ('<':'-':cs) = '\8592':replaceChars cs
replaceChars ('<':'=':cs) = '\8656':replaceChars cs
replaceChars ('\'':cs) = '\8217':replaceChars cs
replaceChars (c:cs) = c:replaceChars cs

pShorthandAttributes :: P Attr
pShorthandAttributes = do
  attr <- mconcat <$>
          many (A.skipWhile isSpace *>
                (Attr [] . uncurry M.singleton <$> pShorthandAttribute))
  A.skipWhile isSpace
  pure attr

pShorthandAttribute :: P (Text,Text)
pShorthandAttribute = do
  let isSpecial c = c == '.' || c == '#' || c == '%' || c == ']' || c ==','
  c <- A.satisfy (\c -> c == '.' || c == '#' || c == '%')
  val <- T.strip <$> A.takeWhile (not . isSpecial)
  key <- case c of
           '.' -> pure "role"
           '#' -> pure "id"
           '%' -> pure "options"
           _ -> mzero
  pure (key, val)

pInline :: [Char] -> P Inline
pInline prevChars = do
  let maybeUnconstrained = case prevChars of
                              (d:_) -> isSpace d || isPunctuation d
                              [] -> True
  let inMatched = pInMatched maybeUnconstrained
  A.skipMany pLineComment
  (do attr <- pFormattedTextAttributes <|> pure mempty
      c <- A.peekChar'
      case c of
        '*' -> inMatched '*' attr (Bold . parseInlines)
        '_' -> inMatched '_' attr (Italic . parseInlines)
        '`' -> inMatched '`' attr (Monospace . parseInlines)
        '#' -> inMatched '#' attr (Span . parseInlines)
        '~' -> pInSingleMatched '~' attr (Subscript . parseInlines)
        '^' -> pInSingleMatched '^' attr (Superscript . parseInlines)
        '+' -> pTriplePassthrough <|> inMatched '+' attr Str
        '"' -> pQuoted '"' attr DoubleQuoted
        '\'' -> pQuoted '\'' attr SingleQuoted
        '(' -> pIndexEntry attr
        _ -> mzero)
     <|> (do c <- A.peekChar'
             case c of
               '\'' -> pApostrophe '\''
               '+' -> pHardBreak
               '{' -> pAttributeReference
               '\\' -> pEscape
               '<' -> pBracedAutolink <|> pCrossReference
               '&' -> pCharacterReference
               '[' -> pBibAnchor <|> pInlineAnchor
               _ | isLetter c -> pInlineMacro <|> pAutolink <|> pEmailAutolink
                 | otherwise -> mzero)

pIndexEntry :: Attr -> P Inline
pIndexEntry attr = do
  void $ A.string "(("
  concealed <- A.option False $ True <$ char '('
  terms <- A.takeWhile1 (/= ')')
  Inline attr <$>
    if concealed
       then IndexEntry (TermConcealed (map T.strip (T.split (==',') terms)))
                         <$ A.string ")))"
       else IndexEntry (TermInText terms) <$ A.string "))"

pTriplePassthrough :: P Inline
pTriplePassthrough = Inline mempty . Passthrough . T.pack
    <$> (A.string "+++" *> A.manyTill A.anyChar (A.string "+++"))

pLineComment :: P ()
pLineComment = A.string "//" *> A.skipWhile (== ' ') *> void pLine

pCrossReference :: P Inline
pCrossReference = do
  void $ A.string "<<"
  t <- T.pack <$> A.manyTill (A.satisfy (not . A.isEndOfLine)) (void (A.string ">>"))
  let ts = T.split (==',') t
  case ts of
    [] -> mzero
    [x] -> pure $ Inline mempty $ CrossReference x Nothing
    (x:xs) -> pure $ Inline mempty $ CrossReference x
                       (Just (parseInlines (T.intercalate "," xs)))

data MatchState = Backslash | OneDelim | Regular
  deriving Show

-- used for super/subscript, which can't accept spaces but take single delims
pInSingleMatched :: Char -> Attr -> (Text -> InlineType) -> P Inline
pInSingleMatched delim attr toInlineType = do
  char delim
  cs <- A.manyTill (A.satisfy (not . isSpace)) (char delim)
  guard $ not $ null cs
  pure $ Inline attr (toInlineType (T.pack cs))

pInMatched :: Bool -> Char -> Attr -> (Text -> InlineType) -> P Inline
pInMatched maybeUnconstrained delim attr toInlineType = do
  char delim
  isDoubled <- A.option False (True <$ char delim)
  followedBySpace <- maybe True isSpace <$> A.peekChar
  guard $ isDoubled || (maybeUnconstrained && not followedBySpace)
  cs <- A.manyTill ( (char '\\' *> A.char delim) <|> A.anyChar )
                   (if isDoubled
                       then char delim *> char delim
                       else char delim)
  guard $ not $ null cs
  when (not isDoubled && maybeUnconstrained) $ do
    mbc <- A.peekChar
    case mbc of
      Nothing -> pure ()
      Just c -> guard $ isSpace c || isPunctuation c
  pure $ Inline attr (toInlineType (T.pack cs))

pInlineAnchor :: P Inline
pInlineAnchor = do
  void $ A.string "[["
  contents <- T.pack <$> A.manyTill A.anyChar (A.string "]]")
  let (anchorId, xrefLabel) =
        case T.split (==',') contents of
          [] -> (mempty, mempty)
          (x:ys) -> (x, mconcat ys)
  pure $ Inline mempty $ InlineAnchor anchorId (parseInlines xrefLabel)

pBibAnchor :: P Inline
pBibAnchor = do
  void $ A.string "[[["
  contents <- T.pack <$> A.manyTill A.anyChar (A.string "]]]")
  let (anchorId, xrefLabel) =
        case T.split (==',') contents of
          [] -> (mempty, mempty)
          (x:ys) -> (x, mconcat ys)
  A.skipWhile (== ' ')
  pure $ Inline mempty $ BibliographyAnchor anchorId (parseInlines xrefLabel)

pCharacterReference :: P Inline
pCharacterReference =
  char '&' *> (pNumericCharacterReference <|> pCharacterEntityReference)

pNumericCharacterReference :: P Inline
pNumericCharacterReference =
  char '#' *> (((char 'x' <|> char 'X') *> pHexReference) <|> pDecimalReference)
 where
  pHexReference =
    Inline mempty . Str . T.singleton . chr <$> (A.hexadecimal <* char ';')
  pDecimalReference =
    Inline mempty . Str . T.singleton . chr <$> (A.decimal <* char ';')

pCharacterEntityReference :: P Inline
pCharacterEntityReference = do
  xs <- A.manyTill (A.satisfy isAlphaNum) (A.char ';' <|> A.space)
  case lookupNamedEntity xs of
    Just s -> pure $ Inline mempty (Str (T.pack s))
    Nothing -> mzero

pQuoted :: Char -> Attr -> ([Inline] -> InlineType) -> P Inline
pQuoted c attr constructor = do
  char c
  result <- pInMatched True '`' attr (constructor . parseInlines)
  char c
  return result

pApostrophe :: Char -> P Inline
pApostrophe '`' = Inline mempty (Str "’") <$ A.string "`'"
pApostrophe _ = mzero

pInlineMacro :: P Inline
pInlineMacro = do
  name <- A.choice (map (\n -> A.string n <* char ':') (M.keys inlineMacros))
  let targetChars = mconcat <$> some
       ( (A.string "pass:" *> char '[' *> A.takeWhile1 (/=']') <* char ']')
         <|>
         A.takeWhile1 (\c -> not (isSpace c) && c /= '[' && c /= '+')
         <|>
         (char '\\' *> (T.singleton <$> A.satisfy (\c -> c == '[' || c == '+')))
         <|>
        (do Inline _ (Str t) <- pInMatched False '+' mempty Str
            pure t)
       )
  target <- mconcat <$> many targetChars
  handleInlineMacro name target

handleInlineMacro :: Text -> Text -> P Inline
handleInlineMacro name target =
  case M.lookup name inlineMacros of
    Nothing -> mzero
    Just f -> f target

inlineMacros :: M.Map Text (Text -> P Inline)
inlineMacros = M.fromList
  [ ("kbd", \_ -> do
       attr <- pAttributes
       let (description, attr') = extractDescription attr
       pure $ Inline attr' $ Kbd (map T.strip (T.split (=='+') description)))
  , ("menu", \target -> do
       attr <- pAttributes
       let (description, attr') = extractDescription attr
       pure $ Inline attr' $ Menu (target : filter (not . T.null)
                                    (map T.strip (T.split (=='>') description))))
  , ("btn", \_ -> do
       attr <- pAttributes
       let (description, attr') = extractDescription attr
       pure $ Inline attr' $ Button description)
  , ("icon", \target -> do
        attr <- pAttributes
        pure $ Inline attr $ Icon target)
  , ("anchor", \target -> do
        attr <- pAttributes
        let (anchorId, xrefLabel) =
              case T.split (==',') target of
                [] -> (mempty, mempty)
                (x:ys) -> (x, mconcat ys)
        pure $ Inline attr $ InlineAnchor anchorId (parseInlines xrefLabel))
  , ("pass", \_ -> do
       attr <- pAttributes
       let (description, attr') = extractDescription attr
       pure $ Inline attr' $ Passthrough description)
  , ("link", \target -> do
      attr <- pAttributes
      let (description, attr') = extractDescription attr
      pure $ Inline attr' $ Link URLLink (Target target)
                                 (if T.null description
                                     then [Inline mempty (Str target)]
                                     else parseInlines description))
  , ("mailto", \target -> do
      attr <- pAttributes
      let (description, attr') = extractDescription attr
      pure $ Inline attr' $ Link EmailLink (Target target)
                                 (if T.null description
                                 then [Inline mempty (Str target)]
                                 else parseInlines description))
  , ("footnote", \target -> do
      attr <- pAttributes
      let (contents, attr') = extractDescription attr
          fnid = if target == mempty
                    then Nothing
                    else Just (FootnoteId target)
      pure $ Inline attr' $ Footnote fnid (parseInlines contents))
  , ("footnoteref", \_ -> do
      (Attr ps kvs) <- pAttributes
      (target, contents) <- case ps of
                                 (t:c:_) -> pure (t,c)
                                 [t] -> pure (t,mempty)
                                 _ -> mzero
      let fnid = if target == mempty
                  then Nothing
                  else Just (FootnoteId target)
      pure $ Inline (Attr mempty kvs) $ Footnote fnid (parseInlines contents))
  , ("xref", \target -> do
        ils <- parseInlines <$> pBracketedText
        let mbtext = if null ils then Nothing else Just ils
        pure $ Inline mempty $ CrossReference target mbtext)
  , ("image", \target -> do
        (Attr ps kvs) <- pAttributes
        let (mbalt, mbw, mbh) =
              case ps of
                (x:y:z:_) -> (Just (AltText x), Width <$> readDecimal y,
                              Height <$> readDecimal z)
                [x,y] -> (Just (AltText x), Width <$> readDecimal y, Nothing)
                [x] -> (Just (AltText x), Nothing, Nothing)
                [] -> (Nothing, Nothing, Nothing)
        pure $ Inline (Attr mempty kvs) $ InlineImage (Target target) mbalt mbw mbh)
  , ("latexmath", \_ ->
      Inline mempty . Math (Just LaTeXMath) <$> pBracketedText)
  , ("asciimath", \_ ->
      Inline mempty . Math (Just AsciiMath) <$> pBracketedText)
  , ("stem", \_ ->
      Inline mempty . Math Nothing <$> pBracketedText)
  , ("indexterm", \_ ->
      Inline mempty . IndexEntry . TermConcealed .
        map T.strip . T.split (==',') <$> pBracketedText)
  , ("indexterm2", \_ ->
      Inline mempty . IndexEntry . TermInText <$> pBracketedText)
  ]

pBracketedText :: P Text
pBracketedText = do
  char '['
  cs <- many ((char '\\' *> A.char ']') <|>
                    A.satisfy (\c -> c /= ']' && not (A.isEndOfLine c)) <|>
                    (' ' <$ (char '\\' <* A.endOfLine)))
  char ']'
  pure $ T.pack cs

extractDescription :: Attr -> (Text, Attr)
extractDescription (Attr ps kvs) =
  let description = case ps of
                      (x:_) -> x
                      _ -> ""
  in (description, Attr (drop 1 ps) kvs)


pEmailAutolink :: P Inline
pEmailAutolink = do
  a <- A.takeWhile1 (\c -> isAlphaNum c || c == '_' || c == '.' || c == '+')
  char '@'
  b <- A.takeWhile1 isLetter
  char '.'
  c <- A.takeWhile1 isLetter
  guard $ let lc = T.length c in lc >= 2 && lc <= 5
  let email = a <> "@" <> b <> "." <> c
  attr <- pAttributes <|> pure mempty
  let (description, attr') = extractDescription attr
  pure $ Inline attr' $ Link EmailLink (Target email)
                             (if T.null description
                                 then [Inline mempty (Str email)]
                                 else parseInlines description)

pAutolink :: P Inline
pAutolink = do
  scheme <- A.choice (map A.string
               ["http:", "https:", "irc:", "ftp:", "mailto:"])
  let isSpecialPunct ',' = True
      isSpecialPunct '.' = True
      isSpecialPunct '?' = True
      isSpecialPunct '!' = True
      isSpecialPunct ':' = True
      isSpecialPunct ';' = True
      isSpecialPunct ')' = True
      isSpecialPunct _ = False
  let urlChunk = T.pack <$>
        some (A.satisfy (\c -> not (isSpace c) && c /= '[' && c /= '>'
                               && not (isSpecialPunct c))
             <|> (do c <- A.satisfy isSpecialPunct
                     mbd <- A.peekChar
                     case mbd of
                       Nothing -> mzero
                       Just d | isSpace d || isSpecialPunct d -> mzero
                       _ -> pure c))
  url <- (scheme <>) . mconcat <$> some
          (urlChunk <|> (do Inline _ (Str t) <- pInMatched False '+' mempty Str
                            pure t))
  attr <- pAttributes <|> pure mempty
  let (description, attr') = extractDescription attr
  pure $ Inline attr' $ Link URLLink (Target url)
                             (if T.null description
                                 then [Inline mempty (Str url)]
                                 else parseInlines description)

pBracedAutolink :: P Inline
pBracedAutolink = char '<' *> pAutolink <* char '>'

pEscape :: P Inline
pEscape =
  -- we allow letters to be escaped to handle escapes of macros
  -- though this also leads to differences from asciidoc
  char '\\' *>
   (Inline mempty . Str . T.singleton <$>
      A.satisfy (\c -> isPunctuation c || isLetter c))

pAttributeReference :: P Inline
pAttributeReference = do
  char '{'
  name <- pDocAttributeName
  char '}'
  case M.lookup name replacements of
    Just r -> pure $ Inline mempty (Str r)
    Nothing -> pure $ Inline mempty $ AttributeReference (AttributeName name)

replacements :: M.Map Text Text
replacements = M.fromList
  [ ("blank", "")
  , ("empty", "")
  , ("sp", " ")
  , ("nbsp", "\160")
  , ("zwsp", "\8203")
  , ("wj", "\8288")
  , ("apos", "\39")
  , ("lsquo", "\8216")
  , ("rsquo", "\8217")
  , ("ldquo", "\8220")
  , ("rdquo", "\8221")
  , ("deg", "\176")
  , ("plus", "+")
  , ("brvbar", "\166")
  , ("vbar", "|")
  , ("amp", "&")
  , ("lt", "<")
  , ("gt", ">")
  , ("startsb", "[")
  , ("endsb", "]")
  , ("caret", "^")
  , ("asterisk", "*")
  , ("tilde", "~")
  , ("backslash", "\\")
  , ("backtick", "`")
  , ("two-colons", "::")
  , ("two-semicolons", ";;")
  , ("cpp", "C++")
  , ("cxx", "C++")
  , ("pp", "++")
  ]

pHardBreak :: P Inline
pHardBreak = do
  char '+'
  _ <- A.takeWhile1 (\c -> c == '\r' || c == '\n')
  pure $ Inline mempty HardBreak

readDecimal :: Text -> Maybe Int
readDecimal t =
  case TR.decimal t of
    Left _ -> Nothing
    Right (x,_) -> Just x

notFollowedBy :: P a -> P ()
notFollowedBy p = optional p >>= guard . isNothing

-- Generate auto-identifiers for sections.

addIdentifiers :: Document -> Document
addIdentifiers doc =
  case M.lookup "sectids" docattr of
    Just _ -> evalState (mapBlocks (addIdentifier prefix idsep) doc) mempty
    Nothing -> doc
 where
  docattr = docAttributes (docMeta doc)
  prefix = fromMaybe "_" $ M.lookup "idprefix" docattr
  idsep = fromMaybe "_" $ M.lookup "idseparator" docattr

addIdentifier :: Text -> Text -> Block -> State (M.Map Text Int) Block
addIdentifier prefix idsep (Block (Attr ps kvs) mbtitle (Section lev ils bs))
  | Nothing <- M.lookup "id" kvs
  = do
      usedIds <- get
      let (ident, usedIds') = generateIdentifier prefix idsep usedIds ils
      put usedIds'
      pure $ Block (Attr ps (M.insert "id" ident kvs)) mbtitle
                     (Section lev ils bs)
addIdentifier _ _ x = pure x

generateIdentifier :: Text -> Text -> M.Map Text Int -> [Inline]
                   -> (Text, M.Map Text Int)
generateIdentifier prefix idsep usedIds ils =
  case M.lookup s usedIds of
    Nothing -> (s, M.insert s 1 usedIds)
    Just n -> (s <> idsep <> T.pack (show (n + 1)), M.insert s (n + 1) usedIds)
 where
  s = prefix <> makeSeps (T.toLower (toString ils))
  makeSeps = T.intercalate idsep . T.words .
               T.map (\case
                       '.' -> ' '
                       '-' -> ' '
                       c | isSpace c -> ' '
                         | otherwise -> c)
  toString = foldInlines getStr
  getStr (Inline _ (Str t)) = t
  getStr _ = ""
