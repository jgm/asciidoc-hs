{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AsciiDoc.AST
  ( Document(..)
  , Meta(..)
  , Author(..)
  , Revision(..)
  , Block(..)
  , BlockType(..)
  , BlockTitle(..)
  , Inline(..)
  , InlineType(..)
  , ListType(..)
  , ListItem(..)
  , CheckboxState(..)
  , ColumnSpec(..)
  , CellStyle(..)
  , TableRow(..)
  , TableCell(..)
  , HorizAlign(..)
  , VertAlign(..)
  , AdmonitionType(..)
  , Target(..)
  , LinkType(..)
  , MathType(..)
  , Attr(..)
  , attrNull
  , Level(..)
  , Language(..)
  , Attribution(..)
  , AltText(..)
  , Width(..)
  , Height(..)
  , FootnoteId(..)
  , AttributeName(..)
  , Callout(..)
  , SourceLine(..)
  ) where

import Data.Text (Text)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Aeson (ToJSON(..), FromJSON(..), genericToEncoding, defaultOptions)

-- | A complete AsciiDoc document
data Document = Document
  { docMeta :: Meta
  , docBlocks :: [Block]
  } deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON Document where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Document

-- | Author information
data Author = Author
  { authorName :: Text
  , authorEmail :: Maybe Text
  } deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON Author where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Author

-- | Revision information
data Revision = Revision
  { revVersion :: Text
  , revDate :: Maybe Text
  , revRemark :: Maybe Text
  } deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON Revision where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Revision

-- | Document metadata
data Meta = Meta
  { docTitle :: [Inline]
  , docAuthors :: [Author]
  , docRevision :: Maybe Revision
  , docAttributes :: Map Text Text
  } deriving (Eq, Generic, Data, Typeable)

instance Show Meta where
  show (Meta [] [] Nothing m) | null m = "Meta [] [] Nothing mempty"
  show (Meta title authors revision attributes) =
    "Meta{ docTitle = " <> show title <>
    ", docAuthors = " <> show authors <>
    ", docRevision = " <> show revision <>
    ", docAttributes = " <> show attributes
    <> "}"

instance ToJSON Meta where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Meta

-- | Attributes attached to an element.
-- The first parameter stores positional attributes in order.
-- The second parameter stores named attributes (including special keys
-- like id/role/options) in a map.
data Attr = Attr [Text] (Map Text Text)
  deriving (Eq, Generic, Data, Typeable)

instance Show Attr where
  show (Attr pos m)
    | null pos && Map.null m = "mempty"
    | otherwise = "Attr " ++ show (pos, m)

instance Semigroup Attr where
  Attr p1 m1 <> Attr p2 m2 =
    let m = m2 <> m1 -- left-biased, favor m2
        m' = (case (Map.lookup "role" m1, Map.lookup "role" m2) of
                (Just x1, Just x2) -> Map.insert "role" (x1 <> " " <> x2)
                _ -> id) .
             (case (Map.lookup "options" m1, Map.lookup "options" m2) of
                (Just x1, Just x2) -> Map.insert "options" (x1 <> "," <> x2)
                _ -> id) $ m
    in  Attr (p1 <> p2) m'

instance Monoid Attr where
  mempty = Attr [] Map.empty
  mappend = (<>)

instance ToJSON Attr where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Attr

attrNull :: Attr -> Bool
attrNull (Attr pos m) = null pos && Map.null m

-- | Nesting or section level
newtype Level = Level Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Level where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Level

-- | Programming or markup language identifier
newtype Language = Language Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Language where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Language

-- | Attribution for quotes
newtype Attribution = Attribution Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Attribution where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Attribution

-- | Alternative text for images
newtype AltText = AltText Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON AltText where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AltText

-- | Width specification in pixels
newtype Width = Width Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Width where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Width

-- | Height specification in pixels
newtype Height = Height Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Height where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Height

-- | Footnote identifier
newtype FootnoteId = FootnoteId Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON FootnoteId where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON FootnoteId

-- | Attribute name
newtype AttributeName = AttributeName Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON AttributeName where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AttributeName

-- | Source line callout
newtype Callout = Callout Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON Callout where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Callout

-- | Source line with possible annotation
data SourceLine =
    SourceLine Text [Callout]
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

instance ToJSON SourceLine where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SourceLine

-- | Block-level element with attributes
data Block = Block Attr (Maybe BlockTitle) BlockType
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON Block where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Block

-- | Block-level element types
data BlockType
  = Section Level [Inline] [Block]
  | DiscreteHeading Level [Inline]
  | Paragraph [Inline]
  | Verse (Maybe Attribution) [Block]
  | LiteralBlock Text
  | Listing (Maybe Language) [SourceLine]
  | IncludeListing (Maybe Language) FilePath (Maybe [SourceLine])
  | ExampleBlock [Block]
  | QuoteBlock (Maybe Attribution) [Block]
  | Sidebar [Block]
  | OpenBlock [Block]
  | PassthroughBlock Text
  | MathBlock (Maybe MathType) Text
  | List ListType [ListItem]
  | DefinitionList [([Inline], [Block])]
  | Table [ColumnSpec] (Maybe [TableRow]) [TableRow] (Maybe [TableRow])
  | BlockImage Target (Maybe AltText) (Maybe Width) (Maybe Height)
  | BlockAudio Target
  | BlockVideo Target
  | TOC
  | Admonition AdmonitionType [Block]
  | PageBreak
  | ThematicBreak
  | Include FilePath (Maybe [Block])
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON BlockType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON BlockType

newtype BlockTitle = BlockTitle [Inline]
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON BlockTitle where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON BlockTitle

-- | Types of admonitions
data AdmonitionType
  = Note
  | Tip
  | Important
  | Caution
  | Warning
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON AdmonitionType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON AdmonitionType

-- | List types
data ListType
  = BulletList Level
  | OrderedList Level
  | CheckList
  | CalloutList
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON ListType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ListType

-- | A list item
data ListItem = ListItem (Maybe CheckboxState) [Block]
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON ListItem where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ListItem

-- | Checkbox state for checklists
data CheckboxState
  = Checked
  | Unchecked
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON CheckboxState where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CheckboxState

-- | Column specification
data ColumnSpec = ColumnSpec
  { colHorizAlign :: Maybe HorizAlign
  , colVertAlign :: Maybe VertAlign
  , colWidth :: Maybe Int
  , colStyle :: Maybe CellStyle
  } deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON ColumnSpec where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON ColumnSpec

-- | Defines how cell contents are parsed
data CellStyle =
    AsciiDocStyle
  | DefaultStyle
  | EmphasisStyle
  | LiteralStyle
  | HeaderStyle
  | MonospaceStyle
  | StrongStyle
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON CellStyle where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON CellStyle

-- | Table row
newtype TableRow = TableRow [TableCell]
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON TableRow where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TableRow

-- | Table cell
data TableCell = TableCell
  { cellContent :: [Block]
  , cellHorizAlign :: Maybe HorizAlign
  , cellVertAlign :: Maybe VertAlign
  , cellColspan :: Int
  , cellRowspan :: Int
  } deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON TableCell where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TableCell

-- | Cell alignment
data HorizAlign
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON HorizAlign where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON HorizAlign

data VertAlign
  = AlignTop
  | AlignMiddle
  | AlignBottom
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON VertAlign where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON VertAlign

-- | Inline element with attributes
data Inline = Inline Attr InlineType
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON Inline where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Inline

-- | Inline element types
data InlineType
  = Str Text
  | HardBreak
  | Bold [Inline]
  | Italic [Inline]
  | Monospace [Inline]
  | Superscript [Inline]
  | Subscript [Inline]
  | Highlight [Inline]
  | Strikethrough [Inline]
  | DoubleQuoted [Inline]
  | SingleQuoted [Inline]
  | Math (Maybe MathType) Text
  | Icon Text
  | Button Text
  | Kbd [Text]
  | Menu [Text]
  | Link LinkType Target [Inline]
  | InlineImage Target (Maybe AltText) (Maybe Width) (Maybe Height)
  | Footnote (Maybe FootnoteId) [Inline]
  | InlineAnchor Text [Inline]
  | BibliographyAnchor Text [Inline]
  | CrossReference Text (Maybe [Inline])
  | AttributeReference AttributeName
  | Span [Inline]
  | Passthrough Text
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON InlineType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InlineType

data MathType
  = AsciiMath
  | LaTeXMath
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON MathType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON MathType

-- | Link types
data LinkType
  = URLLink
  | EmailLink
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON LinkType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LinkType

-- | Link or image target
newtype Target = Target Text
  deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON Target where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Target
