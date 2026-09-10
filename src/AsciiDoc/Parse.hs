{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AsciiDoc.Parse
  ( parseDocument
  ) where

import Prelude hiding (takeWhile)
import Text.HTML.TagSoup.Entity (lookupNamedEntity)
import Data.Maybe (isNothing, listToMaybe, fromMaybe, catMaybes)
import Data.Bifunctor (first)
import Data.Either (lefts, rights)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import Data.List (foldl', intersperse, isPrefixOf, sortOn)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as A (lookAhead)
import System.FilePath
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Data.Char (isAlphaNum, isAscii, isSpace, isLetter, isPunctuation, chr, isDigit,
                  isUpper, isLower, ord)
import AsciiDoc.AST
import AsciiDoc.Generic
-- import Debug.Trace

-- | Parse an AsciiDoc document into an AST.
parseDocument :: Monad m
              => (FilePath -> m Text)
                  -- ^ Get contents of an included file
              -> (FilePath -> Int -> String -> m Document)
                  -- ^ Raise an error given source pos and message
              -> FilePath
                  -- ^ Path of file containing the text
              -> Text -- ^ Text to convert
              -> m Document
parseDocument getFileContents raiseError path t = do
  -- The parser records which constructs occurred, so that the
  -- post-processing passes (each a full traversal of the AST) can be
  -- skipped when they would do nothing.  When includes are expanded,
  -- their contents are not reflected in the flags, so all passes run.
  (doc0, flags) <- case parse ((,) <$> pDocument <*> gets parseFlags) path t of
                     Left err -> do
                       -- raiseError may return a fallback document whose
                       -- contents we know nothing about, so run all passes.
                       d <- raiseError path (errorPosition err)
                                            (errorMessage err)
                       pure (d, ParseFlags True True True True)
                     Right r -> pure r
  doc1 <- if sawInclude flags
             then handleIncludes doc0
             else pure doc0
  let doc2 = if sawInclude flags || sawSection flags
                then addIdentifiers doc1
                else doc1
  doc3 <- if sawInclude flags || sawAttributeReference flags
             then resolveAttributeReferences doc2
             else pure doc2
  if sawInclude flags || sawCrossReference flags
     then resolveCrossReferences doc3
     else pure doc3
 where
  handleResult (Left err) =
    raiseError path (errorPosition err) (errorMessage err)
  handleResult (Right r) = pure r

  toAnchorMap d =
    foldBlocks blockAnchor d <> foldInlines inlineAnchor d

  blockAnchor (Block (Attr _ kvs) _ (Section _ ils _))
    | Just ident <- M.lookup "id" kvs = M.singleton ident ils
  blockAnchor _ = mempty

  inlineAnchor (Inline _ (InlineAnchor ident ils)) = M.singleton ident ils
  inlineAnchor (Inline _ (BibliographyAnchor ident ils)) = M.singleton ident ils
  inlineAnchor _ = mempty

  resolveCrossReferences d = mapInlines (resolveCrossReference (toAnchorMap d)) d
  resolveCrossReference anchorMap
   x@(Inline attr (CrossReference ident Nothing)) =
    let ident' = T.takeWhileEnd (/= '#') ident -- strip off file part
    in case M.lookup ident' anchorMap of
        Just ils -> pure $ Inline attr (CrossReference ident' (Just ils))
        _ -> pure x
  resolveCrossReference _ x = pure x

  resolveAttributeReferences doc =
    mapInlines (goAttref (docAttributes (docMeta doc))) doc

  goAttref atts il@(Inline attr (AttributeReference (AttributeName at))) =
     case M.lookup at atts of
       Nothing -> return il
       Just x -> return $ Inline attr (Str x)
  goAttref _ il = return il

  handleIncludes = mapBlocks (handleIncludeBlock [path])

  -- The first argument is the chain of files being included; a file
  -- that (transitively) includes itself is left unexpanded instead of
  -- recursing forever.
  handleIncludeBlock seen b@(Block attr mbtitle (Include fp Nothing))
    | fp `elem` seen = pure b
    | otherwise =
        (do contents <- getFileContents fp
            Block attr mbtitle . Include fp . Just . docBlocks <$>
              handleResult (parse pDocument fp contents))
          >>= mapBlocks (handleIncludeBlock (fp : seen))
  handleIncludeBlock seen (Block attr mbtitle
                             (IncludeListing mblang fp Nothing)) =
    (do contents <- getFileContents fp
        pure $ Block attr mbtitle $ IncludeListing mblang fp
             $ Just (map (`SourceLine` []) (T.lines contents)))
      >>= mapBlocks (handleIncludeBlock (fp : seen))
  handleIncludeBlock _ x = pure x

-- | Make a relative path relative to a parent's directory.
-- Leaves absolute paths alone.
resolvePath :: FilePath -> FilePath -> FilePath
resolvePath parentPath fp
  | isRelative fp =
      normalise (takeDirectory parentPath </> fp)
  | otherwise = fp

--- Wrapped parser type:

-- A flattened ReaderT ParserConfig (StateT ParserState A.Parser): the
-- config and state are threaded by hand so that primitive operations
-- don't pay for two layers of transformer binds.  As with StateT over a
-- backtracking parser, state changes made by a failed branch of (<|>)
-- are discarded.
newtype P a = P { unP :: ParserConfig -> ParserState
                      -> A.Parser (a, ParserState) }

instance Functor P where
  fmap f (P m) = P $ \c s -> fmap (\(a, s') -> (f a, s')) (m c s)
  {-# INLINE fmap #-}

instance Applicative P where
  pure a = P $ \_ s -> pure (a, s)
  {-# INLINE pure #-}
  P mf <*> P ma = P $ \c s -> do
    (f, s') <- mf c s
    (a, s'') <- ma c s'
    pure (f a, s'')
  {-# INLINE (<*>) #-}
  P ma *> P mb = P $ \c s -> ma c s >>= \(_, s') -> mb c s'
  {-# INLINE (*>) #-}
  P ma <* P mb = P $ \c s -> do
    (a, s') <- ma c s
    (_, s'') <- mb c s'
    pure (a, s'')
  {-# INLINE (<*) #-}

instance Monad P where
  P m >>= f = P $ \c s -> m c s >>= \(a, s') -> unP (f a) c s'
  {-# INLINE (>>=) #-}

instance MonadFail P where
  fail msg = P $ \_ _ -> fail msg

instance Alternative P where
  empty = P $ \_ _ -> empty
  {-# INLINE empty #-}
  P a <|> P b = P $ \c s -> a c s <|> b c s
  {-# INLINE (<|>) #-}

instance MonadPlus P

instance MonadReader ParserConfig P where
  ask = P $ \c s -> pure (c, s)
  {-# INLINE ask #-}
  local f (P m) = P $ \c -> m (f c)
  {-# INLINE local #-}

instance MonadState ParserState P where
  get = P $ \_ s -> pure (s, s)
  {-# INLINE get #-}
  put s = P $ \_ _ -> pure ((), s)
  {-# INLINE put #-}
  state f = P $ \_ s -> pure (f s)
  {-# INLINE state #-}

data ParserState = ParserState
                     { counterMap :: M.Map Text (CounterType, Int)
                     , docAttrs :: M.Map Text Text
                     , parseFlags :: !ParseFlags
                     }
        deriving (Show)

-- | Which constructs occurred during the parse; used to skip
-- post-processing passes that would have no effect.
data ParseFlags = ParseFlags
  { sawInclude :: !Bool
  , sawSection :: !Bool
  , sawAttributeReference :: !Bool
  , sawCrossReference :: !Bool
  } deriving (Show)

noParseFlags :: ParseFlags
noParseFlags = ParseFlags False False False False

setFlag :: (ParseFlags -> ParseFlags) -> P ()
setFlag f = modify $ \s -> s{ parseFlags = f (parseFlags s) }

defaultDocAttrs :: M.Map Text Text
defaultDocAttrs = M.insert "sectids" "" mempty

data ParserConfig = ParserConfig
                    { filePath :: FilePath
                    , blockContexts :: [BlockContext]
                    , hardBreaks :: Bool
                    } deriving (Show)

data ParseError = ParseError { errorPosition :: Int
                             , errorMessage :: String
                             } deriving (Show)

parse :: P a -> FilePath -> T.Text -> Either ParseError a
parse p fp = parse' (ParserConfig{ filePath = fp
                                 , blockContexts = []
                                 , hardBreaks = False
                                 })
                    (ParserState { counterMap = mempty
                                 , docAttrs = defaultDocAttrs
                                 , parseFlags = noParseFlags
                                 })
                    p

parse' :: ParserConfig -> ParserState
       -> P a -> T.Text -> Either ParseError a
parse' cfg st p t =
  go $ A.parse (fst <$> unP p cfg st) t
 where
  go (A.Fail i _ msg) = Left $ ParseError (T.length t - T.length i)
                             $ if "endOfInput" `isPrefixOf` msg
                                  then "Unexpected " <> show (T.take 20 i)
                                  else msg
  go (A.Partial continue) = go (continue "")
  go (A.Done _i r) = Right r

localP :: (ParserConfig -> ParserConfig) -> P a -> P a
localP f (P p) = P $ \c -> p (f c)
{-# INLINE localP #-}

withBlockContext :: BlockContext -> P a -> P a
withBlockContext bc =
  localP (\conf -> conf{ blockContexts = bc : blockContexts conf })

withHardBreaks :: P a -> P a
withHardBreaks = localP (\conf -> conf{ hardBreaks = True })

liftP :: A.Parser a -> P a
liftP p = P $ \_ s -> fmap (\a -> (a, s)) p
{-# INLINE liftP #-}

vchar :: Char -> P ()
vchar = liftP . void . A.char

char :: Char -> P Char
char = liftP . A.char

peekChar :: P (Maybe Char)
peekChar = liftP A.peekChar

peekChar' :: P Char
peekChar' = liftP A.peekChar'

anyChar :: P Char
anyChar = liftP A.anyChar

satisfy :: (Char -> Bool) -> P Char
satisfy = liftP . A.satisfy

space :: P Char
space = liftP A.space

isEndOfLine :: Char -> Bool
isEndOfLine = A.isEndOfLine

match :: P a -> P (T.Text, a)
match p = P $ \c s ->
  (\(t, (x, _)) -> ((t, x), s)) <$> A.match (unP p c s)

-- Like match, but keeps the parser state changes made by the inner
-- parser instead of discarding them.
matchKeepingState :: P a -> P (T.Text, a)
matchKeepingState p = P $ \c s ->
  (\(t, (x, s')) -> ((t, x), s')) <$> A.match (unP p c s)

string :: T.Text -> P T.Text
string = liftP . A.string

decimal :: Integral a => P a
decimal = liftP A.decimal

endOfInput :: P ()
endOfInput = liftP A.endOfInput

endOfLine :: P ()
endOfLine = liftP A.endOfLine

takeWhile :: (Char -> Bool) -> P T.Text
takeWhile f = liftP (A.takeWhile f)

takeWhile1 :: (Char -> Bool) -> P T.Text
takeWhile1 f = liftP (A.takeWhile1 f)

skipWhile :: (Char -> Bool) -> P ()
skipWhile f = liftP (A.skipWhile f)

skipMany :: P a -> P ()
skipMany = A.skipMany

option :: Alternative f => a -> f a -> f a
option = A.option

choice :: [P a] -> P a
choice = A.choice

count :: Int -> P a -> P [a]
count = A.count

manyTill :: P a -> P b  -> P [a]
manyTill = A.manyTill

sepBy :: P a -> P b -> P [a]
sepBy = A.sepBy

sepBy1 :: P a -> P b -> P [a]
sepBy1 = A.sepBy1

--- Block parsing:

data BlockContext =
    SectionContext Int
  | ListContext Char Int
  | DelimitedContext Char Int
  deriving (Show, Eq)


pDocument :: P Document
pDocument = do
  meta <- pDocumentHeader
  attr' <- gets docAttrs
  let minSectionLevel = case M.lookup "doctype" attr' of
                          Just "book" -> 0
                          _ -> 1
  bs <- (case M.lookup "hardbreaks-option" attr' of
            Just "" -> withHardBreaks
            _ -> id) $
        withBlockContext (SectionContext (minSectionLevel - 1)) pBlocks
  skipWhile isSpace
  endOfInput
  attr <- gets docAttrs
  pure $ Document { docMeta = meta{ docAttributes = attr } , docBlocks = bs }

pDocumentHeader :: P Meta
pDocumentHeader = do
  skipBlankLines
  skipMany pDocAttribute
  skipBlankLines
  (title, titleAttr) <- option ([], Nothing) $ do
    (_,titleAttr) <- pTitlesAndAttributes
    title <- pDocumentTitle
    pure (title, case titleAttr of
                   Attr [] kv | M.null kv -> Nothing
                   _ -> Just titleAttr)
  authors <- if null title
                then pure []
                else option [] pDocumentAuthors
  revision <- if null title
                 then pure Nothing
                 else optional pDocumentRevision
  skipMany pDocAttribute
  pure $ Meta{ docTitle = title
             , docTitleAttributes = titleAttr
             , docAuthors = authors
             , docRevision = revision
             , docAttributes = mempty }
              -- docAttributes this gets added at the end from docAttrs in state

pDocumentTitle :: P [Inline]
pDocumentTitle = do
  (vchar '=' <|> vchar '#') <* some (char ' ')
  pLine >>= parseInlines

pDocumentAuthors :: P [Author]
pDocumentAuthors = do
  mbc <- peekChar
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
  vprefix <- option False (True <$ vchar 'v')
  version <- takeWhile1 (\c -> not (isEndOfLine c) && c /= ',')
  date <- optional (T.strip <$> (vchar ',' *> space
               *> takeWhile1 (\c -> not (isEndOfLine c) && c /= ':')))
  remark <- optional
            (T.strip <$> (vchar ':' *>  space *> takeWhile (not . isEndOfLine)))
  endOfLine
  when (isNothing date && isNothing remark) $ guard vprefix
  pure  Revision { revVersion = version
                 , revDate = date
                 , revRemark = remark
                 }

pLine :: P Text
pLine = takeWhile (not . isEndOfLine) <* (endOfLine <|> endOfInput)


pDocAttribute :: P ()
pDocAttribute = do
  vchar ':'
  unset <- option False $ True <$ vchar '!'
  k <- pDocAttributeName
  vchar ':'
  v <- pLineWithEscapes
  modify $ \s ->
    s{ docAttrs =
         if unset
            then M.delete k (docAttrs s)
            else M.insert k v (docAttrs s) }

pDocAttributeName :: P Text
pDocAttributeName = do
  c <- satisfy (\d -> isAscii d && (isAlphaNum d || d == '_'))
  cs <- many $
          satisfy (\d -> isAscii d && (isAlphaNum d || d == '_' || d == '-'))
  pure $ T.pack (c:cs)

pLineWithEscapes :: P Text
pLineWithEscapes = do
  _ <- takeWhile (== ' ')
  t <- takeWhile isLineEndChar
  endOfLine
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

skipBlankLines :: P ()
skipBlankLines = do
  contexts <- asks blockContexts
  case contexts of
    ListContext{} : _ -> skipMany $ vchar '+' *> pBlankLine
    _ -> skipMany pBlankLine

pBlankLine :: P ()
pBlankLine = takeWhile (\c -> c == ' ' || c == '\t') *> (pLineComment <|> endOfLine)

parseWith :: P a -> Text -> P a
parseWith p t = do
  cfg <- ask
  st <- get
  let result = parse' cfg st ((,) <$> p <*> get) t
  case result of
    Left e -> fail $ errorMessage e
    Right (x, newst) -> do
      put newst
      pure x

parseBlocks :: Text -> P [Block]
parseBlocks = parseWith pBlocks . (<> "\n") . T.strip

pBlocks :: P [Block]
pBlocks = do
  bs <- many pBlock
  skipBlankLines
  skipMany pDocAttribute
  skipBlankLines
  pure $ catMaybes bs

parseAsciidoc :: Text -> P Document
parseAsciidoc = parseWith pDocument . (<> "\n") . T.strip

parseParagraphs :: Text -> P [Block]
parseParagraphs = parseWith (many pParagraph) . (<> "\n") . T.strip
 where
  pParagraph = do
    skipBlankLines
    (mbtitle, attr@(Attr _ kvs)) <- pTitlesAndAttributes
    let hardbreaks = M.lookup "options" kvs == Just "hardbreaks"
    skipMany (pCommentBlock attr)
    (if hardbreaks then withHardBreaks else id) $ Block attr mbtitle <$> pPara

parseInlines :: Text -> P [Inline]
parseInlines = parseWith pInlines . T.strip

pBlock :: P (Maybe Block)
pBlock = do
  contexts <- asks blockContexts
  skipBlankLines
  skipMany pDocAttribute
  skipBlankLines
  (mbtitle, attr) <- pTitlesAndAttributes
  case contexts of
    ListContext{} : _ -> skipWhile (== ' ')
    _ -> pure ()
  let hardbreaks =
       case attr of
          Attr _ kvs
            | Just opts <- M.lookup "options" kvs
              -> "hardbreaks" `T.isInfixOf` opts
          _ -> False
  (Nothing <$ pCommentBlock attr) <|> fmap Just
    ((if hardbreaks then withHardBreaks else id) $
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
          choice
            [ pSection
            , pThematicBreak
            , pPageBreak
            , pList
            , pDefinitionList
            , pIndentedLiteral
            , pPara
            ])


pIndentedLiteral :: P BlockType
pIndentedLiteral = do
  xs <- some pIndentedLine
  let minIndent = minimum (map fst xs)
  let xs' = map (first (\x -> x - minIndent)) xs
  let t = T.unlines $ map (\(ind, x) -> T.replicate ind " " <> x) xs'
  pure $ LiteralBlock t

pIndentedLine :: P (Int, Text)
pIndentedLine = do
  ind <- length <$> some (vchar ' ')
  t <- pLine
  pure (ind, t)

pPageBreak :: P BlockType
pPageBreak = PageBreak <$ (string "<<<" <* pBlankLine)

pThematicBreak :: P BlockType
pThematicBreak = ThematicBreak <$
  (pThematicBreakAsciidoc <|> pThematicBreakMarkdown '-' <|> pThematicBreakMarkdown '*')
 where
   pThematicBreakAsciidoc = string "'''" *> pBlankLine
   pThematicBreakMarkdown c = count 3 (vchar c *> many (vchar ' ')) *> pBlankLine

pCommentBlock :: Attr -> P ()
pCommentBlock attr = pDelimitedCommentBlock <|> pAlternateCommentBlock
 where
  pDelimitedCommentBlock = void $ pDelimitedLiteralBlock '/' 4
  pAlternateCommentBlock = do
    case attr of
      Attr ["comment"] _ ->
        void (pDelimitedLiteralBlock '-' 2) <|>
                void (match (withBlockContext (SectionContext (-1)) pPara))
      _ -> mzero

pBlockMacro :: Maybe BlockTitle -> Attr -> P Block
pBlockMacro mbtitle attr = do
  (name, target) <- pBlockMacro'
  handleBlockMacro mbtitle attr name target

pBlockMacro' :: P (Text, Text)
pBlockMacro' = do
  name <- choice (map (\n -> string n <* string "::") (M.keys blockMacros))
  let targetChars = mconcat <$> some
        (takeWhile1 (\c -> not (isSpace c) && c /= '[' && c /= '+')
         <|>
         (vchar '\\' *> (T.singleton <$> satisfy (\c -> c == '[' || c == '+')))
         <|>
         (do Inline _ (Str t) <- pInMatched False '+' mempty (pure . Str)
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
        fp <- asks filePath
        let path = resolvePath fp (T.unpack target)
        setFlag $ \f -> f{ sawInclude = True }
        pure $ Block (attr' <> attr) mbtitle $ Include path Nothing)
  ]

pSection :: P BlockType
pSection = do
  contexts <- asks blockContexts
  case contexts of
    SectionContext sectionLevel : _ -> do
      lev <- (\x -> length x - 1) <$> (some (vchar '=') <|> some (vchar '#'))
      guard (lev > sectionLevel && lev >= 0 && lev <= 5)
      vchar ' '
      title <- pLine >>= parseInlines
      contents <- withBlockContext (SectionContext lev) pBlocks
      -- note: we use sectionLevel, not lev, so in improperly nested content, e.g.,
      -- == foo
      -- ==== bar
      -- ==== baz
      -- bar is a level-3 section and will contain baz!
      setFlag $ \f -> f{ sawSection = True }
      pure $ Section (Level (sectionLevel + 1)) title contents
    _ -> mzero

pDiscreteHeading :: Maybe BlockTitle -> Attr -> P Block
pDiscreteHeading mbtitle attr = do
  let (Attr ps kvs) = attr
  guard $ case ps of
            ("discrete":_) -> True
            _ -> False
  lev <- (\x -> length x - 1) <$> (some (vchar '=') <|> some (vchar '#'))
  guard (lev >= 0 && lev <= 5)
  vchar ' '
  title <- pLine >>= parseInlines
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
    <|> (Right <$> (pAnchor <* endOfLine))
    <|> (Right <$> (pAttributes <* endOfLine))
  ) <* skipMany pBlankLine

pAnchor :: P Attr
pAnchor = do
  void $ string "[["  -- [[anchor]] can set id
  anchor <- takeWhile1 (\c -> not (isEndOfLine c || c == ']' || isSpace c))
  void $ string "]]"
  pure (Attr mempty (M.singleton "id" anchor))

pTitle :: P BlockTitle
pTitle = BlockTitle <$>
           (do vchar '.'
               mbc <- peekChar
               guard $ case mbc of
                         Just ' ' -> False
                         Just '.' -> False
                         _ -> True
               pLineWithEscapes >>= parseInlines)

pDefinitionList :: P BlockType
pDefinitionList =
  DefinitionList <$> some pDefinitionListItem

pDefinitionListItem :: P ([Inline],[Block])
pDefinitionListItem = do
  contexts <- asks blockContexts
  -- The term/definition separator must occur before the end of the
  -- line, so ordinary paragraph text can be rejected with a single
  -- substring check instead of the chunked term scan below.
  restOfLine <- liftP (A.lookAhead (A.takeWhile (not . A.isEndOfLine)))
  guard $ "::" `T.isInfixOf` restOfLine
  let marker = (do t <- takeWhile1 (== ':')
                   case contexts of
                       ListContext ':' n : _ -> guard (T.length t == n + 2)
                       _ -> guard (T.length t == 2)
                   -- The marker must be followed by a space or the end of
                   -- the line, so that e.g. std::vector is not mistaken
                   -- for a term/definition separator.
                   mbc <- peekChar
                   guard $ maybe True (\c -> c == ' ' || isEndOfLine c) mbc)
  skipWhile (== ' ')
  term <- manyTill (takeWhile1 (\c -> not (isEndOfLine c || c == ':'))
                              <|> takeWhile1 (==':')) marker
                    >>= parseInlines . mconcat
  skipWhile (== ' ')
  option () endOfLine
  skipWhile (== ' ')
  let newContext = case contexts of
                      ListContext ':' n : _ -> ListContext ':' (n + 1)
                      _ -> ListContext ':' 1
  defn <- withBlockContext newContext pBlocks
  void $ many pBlankLine
  pure (term, defn)

pList :: P BlockType
pList = do
  (c, lev, mbStart, mbCheckboxState) <- pAnyListItemStart
  let guardContext ctx =
       case ctx of
         ListContext c' lev' -> guard $ c /= c' || lev > lev'
         _ -> pure ()
  asks blockContexts >>= mapM_ guardContext
  ListItem _ bs <- withBlockContext (ListContext c lev) pListItem
  let x = ListItem mbCheckboxState bs
  xs <- many (pListItemStart c lev *> withBlockContext (ListContext c lev) pListItem)
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
  skipWhile (== ' ')
  c <- satisfy (\c -> c == '*' || c == '.' || c == '-' || c == '<')
  lev <- if c == '<'
            then pure 1
            else (+ 1) . T.length <$> takeWhile (== c)
  when (c == '<') $ do  -- callout list <1> or <.>
    void $ string "." <|> takeWhile1 isDigit
    vchar '>'
  vchar ' '
  mbCheck <- if c == '-' || c == '*'
                then optional pCheckbox
                else pure Nothing
  pure (c, lev, Nothing, mbCheck))
 <|> (do d <- decimal
         vchar '.'
         vchar ' '
         pure ('1', 1, Just d, Nothing))


pCheckbox :: P CheckboxState
pCheckbox = do
  skipWhile (==' ')
  vchar '['
  c <- char ' ' <|> char 'x' <|> char '*'
  vchar ']'
  vchar ' '
  pure $ if c == ' '
            then Unchecked
            else Checked

pListItemStart :: Char -> Int -> P ()
pListItemStart c lev = do
  skipWhile (== ' ')
  case c of
    '<' -> vchar '<' *> (string "." <|> takeWhile1 isDigit) *> vchar '>'
    '1' -> do guard (lev == 1)
              void (decimal :: P Int)
              vchar '.'
    _ -> void $ count lev (vchar c)
  vchar ' '

pListItem :: P ListItem
pListItem = do
  mbCheckboxState <- optional pCheckbox
  skipWhile (==' ')
  bs <- pBlocks
  pure $ ListItem mbCheckboxState bs

pDelimitedLiteralBlock :: Char -> Int -> P [T.Text]
pDelimitedLiteralBlock c minimumNumber = do
  len <- length <$> some (vchar c) <* pBlankLine
  guard $ len >= minimumNumber
  -- The bare endOfInput alternative makes an unterminated block extend
  -- to the end of input; without it, manyTill would loop forever at end
  -- of input because pLine succeeds there without consuming anything.
  let endFence = (count len (vchar c) *> (pBlankLine <|> endOfInput))
                 <|> endOfInput
  manyTill pLine endFence

pDelimitedBlock :: Char -> Int -> P [Block]
pDelimitedBlock c minimumNumber = do
  len <- length <$> some (vchar c) <* pBlankLine
  guard $ len >= minimumNumber
  let endFence = count len (vchar c) *> pBlankLine
  withBlockContext (DelimitedContext c len) $
    catMaybes <$> manyTill pBlock endFence

pDelimitedBlockExact :: Char -> Int -> P [Block]
pDelimitedBlockExact c exactNumber = do
  let fence = count exactNumber (vchar c) *> pBlankLine
  fence
  withBlockContext (DelimitedContext c exactNumber) $
    catMaybes <$> manyTill pBlock fence

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
      t <- T.unlines <$> manyTill pLine (pBlankLine <|> endOfInput)
      pure $ Block (Attr ps kvs) mbtitle $ LiteralBlock t
    _ -> mzero

pFenced :: Maybe BlockTitle -> Attr -> P Block
pFenced mbtitle attr = do
  ticks <- takeWhile1 (== '`')
  guard $ T.length ticks >= 3
  lang' <- pLine
  let mblang = case T.strip lang' of
                 "" -> Nothing
                 l -> Just (Language l)
  -- An unterminated block extends to the end of input; without the
  -- endOfInput alternative, manyTill would loop forever at end of input
  -- because pLine succeeds there without consuming anything.  A closing
  -- fence may be longer than the opening one; consume the extra
  -- backticks so they don't leak into the following block.
  lns <- toSourceLines <$>
    manyTill pLine ((string ticks *> skipWhile (== '`')) <|> endOfInput)
  pure $ Block attr mbtitle $ Listing mblang lns

pListing :: Maybe BlockTitle -> Attr -> P Block
pListing mbtitle attr = (do
  let (mbLang, attr') =
        case attr of
          Attr (_:lang:ps) kvs -> (Just (Language lang), Attr ps kvs)
          Attr ["source"] kvs -> (Nothing, Attr [] kvs)
          _ -> (Nothing, attr)
  lns <- toSourceLines <$> pDelimitedLiteralBlock '-' 4
  fp <- asks filePath
  bt <- case lns of
      [SourceLine x []] | "include::" `T.isPrefixOf` x
          , Right ("include", target) <- parse pBlockMacro' fp x
          -> do setFlag $ \f -> f{ sawInclude = True }
                pure $ IncludeListing mbLang
                         (resolvePath fp (T.unpack target)) Nothing
      _ -> pure $ Listing mbLang lns
  pure $ Block attr' mbtitle bt)
 <|>
  (case attr of
    Attr ("listing":ps) kvs -> do
      lns <- toSourceLines <$> manyTill pLine (pBlankLine <|> endOfInput)
      pure $ Block (Attr ps kvs) mbtitle $ Listing Nothing lns
    Attr ("source":lang:ps) kvs -> do
      lns <- toSourceLines <$> manyTill pLine (pBlankLine <|> endOfInput)
      pure $ Block (Attr ps kvs) mbtitle
           $ Listing (Just (Language lang)) lns
    Attr ["source"] kvs -> do
      lns <- toSourceLines <$> manyTill pLine (pBlankLine <|> endOfInput)
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
  bs <- pDelimitedBlock '=' 4
  pure $ case attr of
    Attr (p:ps) kvs |
      Just adm <- parseAdmonitionType p ->
        Block (Attr ps kvs) mbtitle $ Admonition adm bs
    _ -> Block attr mbtitle $ ExampleBlock bs

pSidebar :: Maybe BlockTitle -> Attr -> P Block
pSidebar mbtitle attr =
  Block attr mbtitle . Sidebar <$> pDelimitedBlock '*' 4

pVerse :: Maybe BlockTitle -> Attr -> P Block
pVerse mbtitle (Attr ("verse":xs) kvs) = do
  let attribution = T.intercalate ", " xs
  let mbAttribution = if T.null attribution
                         then Nothing
                         else Just (Attribution attribution)
  bs <- withHardBreaks $
           pDelimitedBlock '-' 2
       <|> pDelimitedBlock '_' 4
       <|> ((:[]) . Block mempty Nothing <$> pPara)
  pure $ Block (Attr [] kvs) mbtitle $ Verse mbAttribution bs
pVerse _ _ = mzero

pQuoteBlock :: Maybe BlockTitle -> Attr -> P Block
pQuoteBlock mbtitle (Attr ("quote":xs) kvs) = do
  let attribution = T.intercalate ", " xs
  let mbAttribution = if T.null attribution
                         then Nothing
                         else Just (Attribution attribution)
  bs <-    pDelimitedBlock '_' 4
       <|> pDelimitedBlock '-' 2
       <|> ((:[]) . Block mempty Nothing <$> pPara)
  pure $ Block (Attr [] kvs) mbtitle $ QuoteBlock mbAttribution bs
pQuoteBlock _ _ = mzero

pOpenBlock :: Maybe BlockTitle -> Attr -> P Block
pOpenBlock mbtitle attr = Block attr mbtitle <$>
  ((OpenBlock <$> pDelimitedBlockExact '-' 2)
   <|>
  (QuoteBlock Nothing <$> (pDelimitedBlock '_' 4)))

parseAdmonitionType :: T.Text -> Maybe AdmonitionType
parseAdmonitionType t =
  case t of
    "NOTE" -> Just Note
    "TIP" -> Just Tip
    "IMPORTANT" -> Just Important
    "CAUTION" -> Just Caution
    "WARNING" -> Just Warning
    _ -> Nothing

pPara :: P BlockType
pPara = do
  t' <- pNormalLine
  contexts <- asks blockContexts
  case contexts of
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
  ts <- many pNormalLine
  hardbreaks <- asks hardBreaks
  ils <- (if hardbreaks
             then newlinesToHardbreaks
             else id) <$> parseInlines (T.unlines (t:ts))
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

pNormalLine :: P Text
pNormalLine = do
  notFollowedBy (string "////" *> pBlankLine) -- block comment
  t <- pLine
  fp <- asks filePath
  guard $ not $ T.all (\c -> c == ' ' || c == '\t') t
  guard $ T.take 1 t /= "[" ||
          case parse (pAttributes *> skipWhile isSpace *> endOfInput)
                     fp t of
                Left _ -> True
                _ -> False
  let t' = T.stripEnd t
  contexts <- asks blockContexts
  let delims = [(c, num) | DelimitedContext c num <- contexts]
  mapM_ (\(c, num) -> guard (not (T.all (== c) t' && T.length t' == num)))
        delims
  case contexts of
    ListContext{} : _ -> do
      guard $ t' /= "+"
      -- A definition list marker is a run of 2 to 4 colons followed by a
      -- space or the end of the line.  A mere "::" infix (e.g. in
      -- std::vector) does not start a definition list.
      let isDlistMarker (_, post) =
            let colons = T.takeWhile (== ':') post
                rest = T.drop (T.length colons) post
            in T.length colons >= 2 && T.length colons <= 4 &&
               (T.null rest || T.head rest == ' ')
      guard $ not $ any isDlistMarker (T.breakOnAll "::" t')
      guard $ case parse pAnyListItemStart fp (T.strip t) of
                Left _ -> True
                _ -> False
    _ -> pure ()
  pure t


--- Table parsing:

pTableBorder :: P TableSyntax
pTableBorder = do
  syntax <- (PSV <$ vchar '|') <|> (DSV <$ vchar ':') <|> (CSV <$ vchar ',')
  void $ string "==="
  skipWhile (=='=')
  pBlankLine
  pure syntax

pTable :: Maybe BlockTitle -> Attr -> P Block
pTable mbtitle (Attr ps kvs) = do
  syntax' <- pTableBorder
  -- Record whether a blank line separates the opening border from the
  -- first row; if so, no header row is implied.
  leadingBlank <- not . null <$> many pBlankLine
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
                            , tableHeader = "header" `elem` options
                            , tableFooter = "footer" `elem` options
                            }
  let getRows mbspecs rowspans = (([],[]) <$ pTableBorder) <|>
         do -- for this row, we modify the specs based on rowspans
            -- if there are rowspans from rows above, we need to skip some:
            let mbspecs' = case mbspecs of
                             Nothing -> Nothing
                             Just specs' -> Just [s | (s,n) <- zip specs' rowspans, n <= 0]
            row@(TableRow cells) <- pTableRow tableOpts mbspecs'
            let numcols = sum (map cellColspan cells)
            let specs = fromMaybe (replicate numcols defaultColumnSpec) mbspecs
            -- now, update rowspans in light of new row; the new row's
            -- cells only occupy the columns that are not taken up by
            -- rowspans from rows above (after decrementing, those
            -- columns have a countdown >= 0), so we skip those:
            let fillRowspans [] rs = rs
                fillRowspans vs (r:rs)
                  | r >= 0 = r : fillRowspans vs rs
                fillRowspans (v:vs) (_:rs) = v : fillRowspans vs rs
                fillRowspans _ [] = []
            let newspans = concatMap
                  (\c -> replicate (cellColspan c) (cellRowspan c - 1)) cells
            let rowspans' = fillRowspans newspans (map (\x -> x - 1) rowspans)
            (\(rows, colspecs') -> (row:rows, case rows of
                                                 [] -> specs
                                                 _ -> colspecs'))
                                     <$> getRows (Just specs) rowspans'
  (rawRows, (rows, colspecs')) <-
    matchKeepingState (getRows mbcolspecs (repeat (0 :: Int)))
  let attr' = Attr ps $ M.delete "format" .
                        M.delete "separator" .
                        M.delete "cols" .
                        M.delete "options" $ kvs
  -- Like Asciidoctor, imply a header row when the first row sits on a
  -- single line directly after the opening border and is followed by a
  -- blank line.
  let isBlankLine = T.all (\c -> c == ' ' || c == '\t')
  let headerImplied = not leadingBlank && not (null rows) &&
        case T.lines rawRows of
          _ : l2 : _ -> isBlankLine l2
          _ -> False
  let hasHeader = tableHeader tableOpts ||
        ("noheader" `notElem` options && headerImplied)
  let (mbHead, rest)
        | hasHeader = (Just (take 1 rows), drop 1 rows)
        | otherwise = (Nothing, rows)
  let (mbFoot, bodyRows)
        | tableFooter tableOpts
        , not (null rest) = (Just (drop (length rest - 1) rest),
                             take (length rest - 1) rest)
        | otherwise = (Nothing, rest)
  pure $ Block attr' mbtitle $ Table colspecs' mbHead bodyRows mbFoot

parseColspecs :: T.Text -> P [ColumnSpec]
parseColspecs t = do
  fp <- asks filePath
  case parse pColspecs fp t of
    Left e -> fail $ errorMessage e
    Right cs -> pure cs

pColspecs :: P [ColumnSpec]
pColspecs = mconcat <$> sepBy pColspecPart pComma <* option () pComma

pColspecPart :: P [ColumnSpec]
pColspecPart = do
  multiplier <- option 1 pMultiplier
  replicate multiplier <$> pColspec

pMultiplier :: P Int
pMultiplier = decimal <* vchar '*'

pColspec :: P ColumnSpec
pColspec = ColumnSpec <$> optional pHorizAlign
                      <*> optional pVertAlign
                      <*> (pWidth <|> pure Nothing)
                      <*> (toCellStyle <$> satisfy (A.inClass "adehlms")
                             <|> pure Nothing)

pHorizAlign :: P HorizAlign
pHorizAlign =
  (AlignLeft <$ vchar '<') <|> (AlignCenter <$ vchar '^') <|> (AlignRight <$ vchar '>')

pVertAlign :: P VertAlign
pVertAlign = do
  vchar '.'
  (AlignTop <$ vchar '<') <|> (AlignMiddle <$ vchar '^') <|> (AlignBottom <$ vchar '>')

pWidth :: P (Maybe Int)
pWidth = (Just <$> (decimal <* option () (vchar '%'))) <|> (Nothing <$ vchar '~')

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
                   skipMany pBlankLine
                   (xs ++) <$> getCell (drop (sum (map cellColspan xs)) colspecs')
             in  getCell colspecs
         | otherwise -> mconcat <$>
               some (pTableCellPSV (tableSeparator opts)
                       False (repeat defaultColumnSpec))
                     <* skipMany pBlankLine
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
  as <- sepBy (pCSVCell delim) (vchar delim)
  pBlankLine *> skipMany pBlankLine
  zipWithM toBasicCell as (colspecs ++ repeat defaultColumnSpec)

pCSVCell :: Char -> P T.Text
pCSVCell delim = do
  skipWhile (== ' ')
  mbc <- peekChar
  case mbc of
    Just '"'
      -> vchar '"' *>
          (T.pack <$>
            manyTill (satisfy (/='"') <|> ('"' <$ string "\"\"")) (vchar '"'))
    _ -> T.strip . T.replace "\"\"" "\"" <$>
           takeWhile (\c -> c /= delim && not (isEndOfLine c))

-- no "; escape delim with backslash
pDSVTableRow:: Char -> Maybe [ColumnSpec] -> P [TableCell]
pDSVTableRow delim mbcolspecs = do
  let colspecs = fromMaybe [] mbcolspecs
  as <- sepBy (pDSVCell delim) (vchar delim)
  pBlankLine *> skipMany pBlankLine
  zipWithM toBasicCell as (colspecs ++ repeat defaultColumnSpec)

pDSVCell :: Char -> P T.Text
pDSVCell delim =
  T.strip . mconcat <$>
    many (takeWhile1 (\c -> c /= delim && c /= '\\' && not (isEndOfLine c))
       <|> (vchar '\\' *> ((\c -> "\\" <> T.singleton c) <$> anyChar)))

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
  -- A cell separator match can only begin at the separator itself or
  -- at one of the characters that may precede it in a cell spec
  -- (whitespace, duplicate/span numbers, alignments, styles); a table
  -- border only at its delimiter.  Runs of other characters can be
  -- consumed at once, and the expensive lookahead for a separator or
  -- border is only needed at characters that could begin one.
  let couldStartCellSep c = c == sep || c == ' ' || c == '\t' ||
        isDigit c || A.inClass ".<^>adehlms" c
  let couldStartBorder c = c == '|' || c == ':' || c == ','
  let isPlainCellChar c = not (couldStartCellSep c) &&
        not (couldStartBorder c) && c /= '\\' && not (isEndOfLine c)
  t <- mconcat <$>
         many
          (takeWhile1 isPlainCellChar
           <|>
           (do mbc <- peekChar
               case mbc of
                 Nothing -> mzero
                 Just c -> do
                   when (couldStartCellSep c) $
                     notFollowedBy (void (pCellSep sep))
                   when (couldStartBorder c) $
                     notFollowedBy (void pTableBorder)
                   -- pCellSep skips leading whitespace itself, so if it
                   -- failed at the first space of a run it fails at
                   -- every position within it; the whole run can be
                   -- consumed after a single lookahead.
                   if c == ' ' || c == '\t'
                      then takeWhile1 (\d -> d == ' ' || d == '\t')
                      else T.singleton <$>
                        ((vchar '\\' *> char sep)
                          <|> satisfy (not . isEndOfLine)
                          <|> if allowNewlines
                                 then satisfy isEndOfLine
                                 else satisfy isEndOfLine
                                        <* notFollowedBy (pCellSep sep))))
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
    AsciiDocStyle -> docBlocks <$> parseAsciidoc t
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
  skipWhile (\c -> c == ' ' || c == '\t')
  -- Fail fast unless the next character can actually begin a cell
  -- separator, so that speculative lookaheads stay cheap.
  mbc <- peekChar
  case mbc of
    Just c | c == sep || isDigit c || A.inClass ".<^>adehlms" c -> pure ()
    _ -> mzero
  mult <- option 1 pMultiplier
  (colspan, rowspan) <- option (Nothing, Nothing) $ do
    a <- optional decimal
    b <- optional $ vchar '.' *> decimal
    guard $ not (isNothing a && isNothing b)
    vchar '+'
    pure (a, b)
  halign <- optional pHorizAlign
  valign <- optional pVertAlign
  sty <- (toCellStyle <$> satisfy (A.inClass "adehlms")) <|> pure Nothing
  notFollowedBy pTableBorder <* vchar sep
  pure $ CellData
    { cDuplicate = mult
    , cHorizAlign = halign
    , cVertAlign = valign
    , cColspan = colspan
    , cRowspan = rowspan
    , cStyle = sty
    }


--- Inline parsing:

pInlines :: P [Inline]
pInlines = pInlines' []

pComma :: P ()
pComma = vchar ',' <* skipWhile isSpace

pFormattedTextAttributes :: P Attr
pFormattedTextAttributes = do
  vchar '['
  as <- pShorthandAttributes
  ps <- option []
         (do unless (as == mempty) pComma
             sepBy1 pAttributeValue pComma <* option () pComma)
  vchar ']'
  if as == mempty
     then
       case ps of
         [] -> pure mempty
         (x:_) -> pure $ Attr [] (M.fromList [("role",x)])
     else pure as

pAttributes :: P Attr
pAttributes = do
  vchar '['
  (xs, as) <- option ([], mempty) $ do
    x <- takeWhile (\c -> isAlphaNum c || c == '-' || c == '_')
    as <- pShorthandAttributes
    case as of
       Attr [] m | M.null m -> mzero
       _ -> pure ([x | not (T.null x)] , as)
  bs <- option []
         (do unless (as == mempty) pComma
             sepBy pAttribute pComma <* option () pComma)
  vchar ']'
  let positional = xs ++ lefts bs
  let kvs = rights bs
  pure $ as <> Attr positional (M.fromList kvs)

pAttribute :: P (Either Text (Text,Text))
pAttribute = (Right <$> pKeyValue) <|> (Left <$> pPositional)

pKeyValue :: P (Text, Text)
pKeyValue = do
  k <- takeWhile1 (\c -> c /= ',' && c /= ']' && c /= '=')
  vchar '=' *> ((k,) <$> pAttributeValue)

pPositional :: P Text
pPositional = do
  v <- pAttributeValue
  mbc <- peekChar
  case mbc of
    Just ',' -> pure ()
    _ -> guard $ not $ T.null v
  pure v

pAttributeValue :: P Text
pAttributeValue = pQuotedAttr <|> pBareAttributeValue
 where
   pBareAttributeValue =
     T.strip <$> takeWhile (\c -> c /= ',' && c /= ']')

pQuotedAttr :: P Text
pQuotedAttr = do
   vchar '"'
   result <- many (satisfy (/='"') <|> (vchar '\\' *> satisfy (/='"')))
   vchar '"'
   pure $ T.pack result

-- The [Text] argument accumulates the plain text seen so far, in
-- reverse chunk order; prependStr turns it into a Str inline.
pInlines' :: [Text] -> P [Inline]
pInlines' cs = do
  -- Dispatch on the next character: only try the parsers that can
  -- possibly succeed there, instead of running every alternative (and
  -- paying for its failure) at each position.
  mbc <- peekChar
  case mbc of
    Nothing -> pure $ addStr []
    Just c
      | isLetter c -> pLetterRun cs
      | c == '/' -> (pLineComment *> pInlines' cs) <|> pPlainChar c
      | isInlineStartChar c ->
          (do il' <- pInline cs
              let il = case il' of
                         Inline (Attr ps kvs) (Span ils)
                           | Nothing <- M.lookup "role" kvs
                           -> Inline (Attr ps kvs) (Highlight ils)
                         _ -> il'
              addStr . (il:) <$> pInlines' [])
          <|> pPlainChar c
      | otherwise -> do
          inert <- takeWhile1 isInertChar
          pInlines' (inert : cs)
 where
  addStr = prependStr cs
  -- Consume a character that failed to start an inline element (or
  -- line comment), along with any following inert run.
  pPlainChar c = do
    _ <- anyChar
    inert <- takeWhile isInertChar
    pInlines' (T.cons c inert : cs)

-- Characters that can begin an inline element (see pInline).
isInlineStartChar :: Char -> Bool
isInlineStartChar c = elem c ("*_`#~^+\"'({\\<&[" :: [Char])

-- Characters that cannot begin any inline element or line comment, so
-- a run of them can be consumed at once without retrying the inline
-- parsers at each position.
isInertChar :: Char -> Bool
isInertChar c =
  not (isLetter c) && not (isInlineStartChar c) && c /= '/'

prependStr :: [Text] -> [Inline] -> [Inline]
prependStr [] = id
prependStr cs =
  (Inline mempty (Str (replaceCharsText (T.concat (reverse cs)))):)

-- Apply replaceChars, going through String only when the text can
-- actually contain the start of a replacement.
replaceCharsText :: Text -> Text
replaceCharsText t
  | mayNeedReplacement t = T.pack (replaceChars (T.unpack t))
  | otherwise = t

-- Whether any replaceChars pattern could match: one of its trigger
-- characters occurs ('.' only counts in a pair, since it is only
-- rewritten as part of "...").
mayNeedReplacement :: Text -> Bool
mayNeedReplacement = snd . T.foldl' step (False, False)
 where
  step acc@(_, True) _ = acc
  step (prevDot, _) c
    | c == '.' = (True, prevDot)
    | otherwise = (False, c == '(' || c == '-' || c == '=' ||
                          c == '<' || c == '\'')

-- The only inline elements that can start at a letter are macros,
-- autolinks and email autolinks, so a whole run of letters can be
-- consumed at once and just those candidates tried.  A macro or
-- autolink name is followed by ':' (possibly after a non-letter rest
-- of the name, like the '2' of indexterm2), which ends the run, so
-- its letter-only prefix must be a suffix of the run; only those few
-- suffix positions need to be tried, leftmost first.  An email
-- autolink can only usefully start at the beginning of the run, since
-- every letter is a valid local-part character and the greedy
-- local-part scan ends at the same place either way.  This makes
-- parsing a run of letters linear instead of quadratic in its length.
pLetterRun :: [Text] -> P [Inline]
pLetterRun cs = do
  w <- takeWhile1 isLetter
  let plain = pInlines' (w : cs)
  let candidates = [ x | x@(letters, _, _) <-
                           M.findWithDefault [] (T.last w) nestedInlineStartsByLastLetter
                       , letters `T.isSuffixOf` w ]
  let (atStart, nested) =
        span (\(letters, _, _) -> T.length letters == T.length w) candidates
  let tryEmail = do
        more <- takeWhile isEmailLocalChar
        il <- pEmailAutolinkRest (w <> more)
        prependStr cs . (il:) <$> pInlines' []
  -- An email autolink continues with more local-part characters or the
  -- '@'; anything else after the letter run rules it out, so the
  -- attempt (and its backtracking) can be skipped.
  mbNext <- peekChar
  let emailPossible = case mbNext of
        Just d -> d == '@' || isEmailLocalChar d
        Nothing -> False
  let withEmail p = if emailPossible then tryEmail <|> p else p
  foldr (tryNested w) (withEmail (foldr (tryNested w) plain nested)) atStart
 where
  tryNested w (letters, nameRest, p) alt =
    (do void $ string (nameRest <> ":")
        il <- p
        let pre = T.dropEnd (T.length letters) w
        let cs' = if T.null pre then cs else pre : cs
        prependStr cs' . (il:) <$> pInlines' [])
    <|> alt

-- Possible starts of macros and autolinks, as (letter-only prefix of
-- the name, rest of the name, parser for what follows the name and
-- ':').  Sorted by decreasing prefix length, i.e. by increasing
-- starting position within a run of letters, so that the leftmost
-- match wins; macros come before autolink schemes of the same name.
nestedInlineStarts :: [(Text, Text, P Inline)]
nestedInlineStarts =
  sortOn (\(letters, _, _) -> negate (T.length letters)) $
    [ (letters, T.drop (T.length letters) name, pInlineMacroTarget name)
    | name <- M.keys inlineMacros
    , let letters = T.takeWhile isLetter name
    ] ++
    [ (scheme, "", pAutolinkTarget (scheme <> ":"))
    | scheme <- autolinkSchemes
    ]

-- The same starts indexed by the last letter of the prefix, so that a
-- letter run only has to check the few candidates that could be its
-- suffix, preserving the order of nestedInlineStarts within a bucket.
nestedInlineStartsByLastLetter :: M.Map Char [(Text, Text, P Inline)]
nestedInlineStartsByLastLetter =
  M.fromListWith (flip (++))
    [ (T.last letters, [x]) | x@(letters, _, _) <- nestedInlineStarts ]

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
          many (skipWhile isSpace *>
                (Attr [] . uncurry M.singleton <$> pShorthandAttribute))
  skipWhile isSpace
  pure attr

pShorthandAttribute :: P (Text,Text)
pShorthandAttribute = do
  let isSpecial c = c == '.' || c == '#' || c == '%' || c == ']' || c ==','
  c <- satisfy (\c -> c == '.' || c == '#' || c == '%')
  val <- T.strip <$> takeWhile (not . isSpecial)
  key <- case c of
           '.' -> pure "role"
           '#' -> pure "id"
           '%' -> pure "options"
           _ -> mzero
  pure (key, val)

pInline :: [Text] -> P Inline
pInline prevChars = do
  -- The chunks in prevChars are non-empty by construction.
  let maybeUnconstrained = case prevChars of
                              (t:_) -> let d = T.last t
                                       in isSpace d || isPunctuation d || d == '+'
                              [] -> True
  let inMatched = pInMatched maybeUnconstrained
  (do attr <- pFormattedTextAttributes <|> pure mempty
      c <- peekChar'
      case c of
        '*' -> inMatched '*' attr (fmap Bold . parseInlines)
        '_' -> inMatched '_' attr (fmap Italic . parseInlines)
        '`' -> inMatched '`' attr (fmap Monospace . parseInlines)
        '#' -> inMatched '#' attr (fmap Span . parseInlines)
        '~' -> pInSingleMatched '~' attr (fmap Subscript . parseInlines)
        '^' -> pInSingleMatched '^' attr (fmap Superscript . parseInlines)
        '+' -> pTriplePassthrough <|> inMatched '+' attr (pure . Str)
        '"' -> pQuoted '"' attr DoubleQuoted
        '\'' -> pQuoted '\'' attr SingleQuoted
        '(' -> pIndexEntry attr
        _ -> mzero)
     <|> (do c <- peekChar'
             case c of
               '\'' -> pApostrophe '\''
               '+' -> pHardBreak
               '{' -> pCounter <|> pAttributeReference
               '\\' -> pEscape
               '<' -> pBracedAutolink <|> pCrossReference
               '&' -> pCharacterReference
               '[' -> pBibAnchor <|> pInlineAnchor
               -- letters are handled by pLetterRun
               _ -> mzero)

pIndexEntry :: Attr -> P Inline
pIndexEntry attr = do
  void $ string "(("
  concealed <- option False $ True <$ vchar '('
  terms <- takeWhile1 (/= ')')
  Inline attr <$>
    if concealed
       then IndexEntry (TermConcealed (map T.strip (T.split (==',') terms)))
                         <$ string ")))"
       else IndexEntry (TermInText terms) <$ string "))"

pTriplePassthrough :: P Inline
pTriplePassthrough = Inline mempty . Passthrough . T.pack
    <$> (string "+++" *> manyTill anyChar (string "+++"))

pLineComment :: P ()
pLineComment = string "//" *> satisfy (\c -> c == ' ' || c == '\t') *> void pLine

pCrossReference :: P Inline
pCrossReference = do
  void $ string "<<"
  t <- T.pack <$> manyTill (satisfy (not . isEndOfLine)) (void (string ">>"))
  let ts = T.split (==',') t
  case ts of
    [] -> mzero
    [x] -> do
      setFlag $ \f -> f{ sawCrossReference = True }
      pure $ Inline mempty $ CrossReference x Nothing
    (x:xs) -> Inline mempty . CrossReference x . Just
                       <$> parseInlines (T.intercalate "," xs)

data MatchState = Backslash | OneDelim | Regular
  deriving Show

-- used for super/subscript, which can't accept spaces but take single delims
pInSingleMatched :: Char -> Attr -> (Text -> P InlineType) -> P Inline
pInSingleMatched delim attr toInlineType = do
  vchar delim
  cs <- manyTill (satisfy (not . isSpace)) (vchar delim)
  guard $ not $ null cs
  Inline attr <$> toInlineType (T.pack cs)

pInMatched :: Bool -> Char -> Attr -> (Text -> P InlineType) -> P Inline
pInMatched maybeUnconstrained delim attr toInlineType = do
  vchar delim
  isDoubled <- option False (True <$ vchar delim)
  followedBySpace <- maybe True isSpace <$> peekChar
  guard $ isDoubled || (maybeUnconstrained && not followedBySpace)
  t <- pMatchedContent isDoubled delim
  guard $ not $ T.null t
  when (not isDoubled && maybeUnconstrained) $ do
    mbc <- peekChar
    case mbc of
      Nothing -> pure ()
      Just c -> guard $ isSpace c || isPunctuation c || c == '+'
  Inline attr <$> toInlineType t

-- Scan the content of a delimited span up to and including the closing
-- delimiter (doubled or single), consuming runs of plain characters in
-- chunks rather than character by character.  A backslash escapes the
-- delimiter; in doubled mode a lone delimiter is content.
pMatchedContent :: Bool -> Char -> P Text
pMatchedContent isDoubled delim = mconcat <$> go
 where
  closing = if isDoubled
               then vchar delim *> vchar delim
               else vchar delim
  go = ([] <$ closing) <|> ((:) <$> piece <*> go)
  piece = takeWhile1 (\c -> c /= delim && c /= '\\')
      <|> (vchar '\\' *> ((T.singleton <$> char delim) <|> pure "\\"))
      <|> (T.singleton <$> char delim)
          -- only reachable in doubled mode, when the delimiter is not
          -- part of a closing pair

pInlineAnchor :: P Inline
pInlineAnchor = do
  void $ string "[["
  contents <- T.pack <$> manyTill anyChar (string "]]")
  let (anchorId, xrefLabel) =
        case T.split (==',') contents of
          [] -> (mempty, mempty)
          (x:ys) -> (x, mconcat ys)
  Inline mempty . InlineAnchor anchorId <$> parseInlines xrefLabel

pBibAnchor :: P Inline
pBibAnchor = do
  void $ string "[[["
  contents <- T.pack <$> manyTill anyChar (string "]]]")
  let (anchorId, xrefLabel) =
        case T.split (==',') contents of
          [] -> (mempty, mempty)
          (x:ys) -> (x, mconcat ys)
  skipWhile (== ' ')
  Inline mempty . BibliographyAnchor anchorId <$> parseInlines xrefLabel

pCharacterReference :: P Inline
pCharacterReference =
  vchar '&' *> (pNumericCharacterReference <|> pCharacterEntityReference)

pNumericCharacterReference :: P Inline
pNumericCharacterReference =
  vchar '#' *> (((vchar 'x' <|> vchar 'X') *> pHexReference) <|> pDecimalReference)
 where
  pHexReference =
    Inline mempty . Str . T.singleton . chr <$> (liftP A.hexadecimal <* vchar ';')
  pDecimalReference =
    Inline mempty . Str . T.singleton . chr <$> (decimal <* vchar ';')

pCharacterEntityReference :: P Inline
pCharacterEntityReference = do
  xs <- manyTill (satisfy isAlphaNum) (char ';')
  case lookupNamedEntity xs of
    Just s -> pure $ Inline mempty (Str (T.pack s))
    Nothing -> mzero

pQuoted :: Char -> Attr -> ([Inline] -> InlineType) -> P Inline
pQuoted c attr constructor = do
  vchar c
  result <- pInMatched True '`' attr (fmap constructor . parseInlines)
  vchar c
  return result

pApostrophe :: Char -> P Inline
pApostrophe '`' = Inline mempty (Str "’") <$ string "`'"
pApostrophe _ = mzero

-- Parse the part of an inline macro after the name and ':'.
pInlineMacroTarget :: Text -> P Inline
pInlineMacroTarget name = do
  let targetChars = mconcat <$> some
       ( (string "pass:" *> vchar '[' *> takeWhile1 (/=']') <* vchar ']')
         <|>
         takeWhile1 (\c -> not (isSpace c) && c /= '[' && c /= '+')
         <|>
         (vchar '\\' *> (T.singleton <$> satisfy (\c -> c == '[' || c == '+')))
         <|>
        (do Inline _ (Str t) <- pInMatched False '+' mempty (pure . Str)
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
        Inline attr . InlineAnchor anchorId <$> parseInlines xrefLabel)
  , ("pass", \_ -> do
       attr <- pAttributes
       let (description, attr') = extractDescription attr
       pure $ Inline attr' $ Passthrough description)
  , ("link", \target -> do
      attr <- pAttributes
      let (description, attr') = extractDescription attr
      Inline attr' . Link URLLink (Target target)
          <$> (if T.null description
                  then pure [Inline mempty (Str target)]
                  else parseInlines description))
  , ("mailto", \target -> do
      attr <- pAttributes
      let (description, attr') = extractDescription attr
      Inline attr' . Link EmailLink (Target target)
             <$> if T.null description
                    then pure [Inline mempty (Str target)]
                    else parseInlines description)
  , ("footnote", \target -> do
      ils <- pBracketedText >>= parseInlines
      let fnid = if target == mempty
                    then Nothing
                    else Just (FootnoteId target)
      pure $ Inline mempty (Footnote fnid ils))
  , ("footnoteref", \_ -> do
      (Attr ps kvs) <- pAttributes
      (target, contents) <- case ps of
                                 (t:c:_) -> pure (t,c)
                                 [t] -> pure (t,mempty)
                                 _ -> mzero
      let fnid = if target == mempty
                  then Nothing
                  else Just (FootnoteId target)
      Inline (Attr mempty kvs) . Footnote fnid <$> parseInlines contents)
  , ("xref", \target -> do
        ils <- pBracketedText >>= parseInlines
        mbtext <- if null ils
                     then Nothing <$ setFlag (\f -> f{ sawCrossReference = True })
                     else pure (Just ils)
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
pBracketedText =
  vchar '[' *>
    (mconcat <$> many
         (T.pack <$> some ((vchar '\\' *> char ']') <|>
                           (vchar '+' *> vchar '+' *> char ']'
                             <* vchar '+' <* vchar '+') <|>
                 satisfy (\c -> c /= ']' && c /= '[' && not (isEndOfLine c)) <|>
                 (' ' <$ (vchar '\\' <* endOfLine)))
          <|> ((\x -> "[" <> x <> "]") <$> pBracketedText)))
    <* vchar ']'

extractDescription :: Attr -> (Text, Attr)
extractDescription (Attr ps kvs) =
  let description = case ps of
                      (x:_) -> x
                      _ -> ""
  in (description, Attr (drop 1 ps) kvs)


isEmailLocalChar :: Char -> Bool
isEmailLocalChar c = isAlphaNum c || c == '_' || c == '.' || c == '+'

-- Parse the part of an email autolink after the local part.
pEmailAutolinkRest :: Text -> P Inline
pEmailAutolinkRest a = do
  vchar '@'
  b <- takeWhile1 isLetter
  vchar '.'
  c <- takeWhile1 isLetter
  guard $ let lc = T.length c in lc >= 2 && lc <= 5
  let email = a <> "@" <> b <> "." <> c
  (description, attr') <- pLinkDescription <|> pure (mempty, mempty)
  Inline attr' . Link EmailLink (Target email)
           <$> if T.null description
                  then pure [Inline mempty (Str email)]
                  else parseInlines description

autolinkSchemes :: [Text]
autolinkSchemes = ["http", "https", "irc", "ftp", "mailto"]

pAutolink :: P Inline
pAutolink = do
  scheme <- choice (map (\s -> string (s <> ":")) autolinkSchemes)
  pAutolinkTarget scheme

-- Parse the part of an autolink after the scheme (which includes the
-- trailing ':').
pAutolinkTarget :: Text -> P Inline
pAutolinkTarget scheme = do
  let isSpecialPunct ',' = True
      isSpecialPunct '.' = True
      isSpecialPunct '?' = True
      isSpecialPunct '!' = True
      isSpecialPunct ':' = True
      isSpecialPunct ';' = True
      isSpecialPunct ')' = True
      isSpecialPunct _ = False
  let urlChunk = T.pack <$>
        some (satisfy (\c -> not (isSpace c) && c /= '[' && c /= '>'
                               && not (isSpecialPunct c))
             <|> (do c <- satisfy isSpecialPunct
                     mbd <- peekChar
                     case mbd of
                       Nothing -> mzero
                       Just d | isSpace d || isSpecialPunct d -> mzero
                       _ -> pure c))
  url <- (scheme <>) . mconcat <$> some
          (urlChunk <|> (do Inline _ (Str t) <- pInMatched False '+' mempty (pure . Str)
                            pure t))
  (description, attr') <- pLinkDescription <|> pure (mempty, mempty)
  Inline attr' . Link URLLink (Target url)
             <$> if T.null description
                    then pure [Inline mempty (Str url)]
                    else parseInlines description

pLinkDescription :: P (Text, Attr)
pLinkDescription = do
  vchar '['
  -- parse link description
  let literalPart = takeWhile1 (\c -> c /= ']' && c /= ',' && c /= '=')
  description <- option mempty $
                    pQuotedAttr
                <|> mconcat <$>
                     many (literalPart
                         <|> ("," <$ vchar ',' <* notFollowedBy pKeyValue))
  -- parse (optional) attributes
  kvs <- option []
         (do unless (description == mempty) pComma
             sepBy pKeyValue pComma <* option () pComma)
  vchar ']'
  pure (description, Attr mempty (M.fromList kvs))


pBracedAutolink :: P Inline
pBracedAutolink = vchar '<' *> pAutolink <* vchar '>'

pEscape :: P Inline
pEscape =
  -- we allow letters to be escaped to handle escapes of macros
  -- though this also leads to differences from asciidoc
  vchar '\\' *>
   (Inline mempty . Str . T.singleton <$>
      satisfy (\c -> isPunctuation c || isLetter c))

pCounter :: P Inline
pCounter = do
  vchar '{' <* string "counter:"
  name <- pDocAttributeName
  mbvalue <- optional (vchar ':' *> pCounterValue)
  vchar '}'
  cmap <- gets counterMap
  let (ctype, val) =
        case M.lookup name cmap of
          Just (ctype', val') -> (ctype', val' + 1)
          Nothing ->
            case mbvalue of
              Nothing -> (DecimalCounter, 1)
              Just (ctype', val') -> (ctype', val')
  modify $ \st -> st{ counterMap =
                       M.insert name (ctype, val) (counterMap st) }
  pure $ Inline mempty $ Counter name ctype val

pCounterValue :: P (CounterType, Int)
pCounterValue = pUpperValue <|> pLowerValue <|> pDecimalValue
 where
   pUpperValue = do
     c <- satisfy (\c -> isAscii c && isUpper c)
     pure (UpperAlphaCounter, 1 + (ord c - ord 'A'))
   pLowerValue = do
     c <- satisfy (\c -> isAscii c && isLower c)
     pure (LowerAlphaCounter, 1 + (ord c - ord 'a'))
   pDecimalValue = do
     n <- decimal
     pure (DecimalCounter, n)

pAttributeReference :: P Inline
pAttributeReference = do
  vchar '{'
  name <- pDocAttributeName
  vchar '}'
  case M.lookup name replacements of
    Just r -> pure $ Inline mempty (Str r)
    Nothing -> do
      -- Resolve document attributes at the point of use, so that a
      -- reference sees the value in effect where it occurs.  References
      -- to attributes defined later are left unresolved here and get
      -- the end-of-document value in a post-processing pass.
      attrs <- gets docAttrs
      case M.lookup name attrs of
        Just v -> pure $ Inline mempty (Str v)
        Nothing -> do
          setFlag $ \f -> f{ sawAttributeReference = True }
          pure $ Inline mempty $ AttributeReference (AttributeName name)

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
  vchar '+'
  _ <- takeWhile1 (\c -> c == '\r' || c == '\n')
  pure $ Inline mempty HardBreak

--- Utility functions:

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
