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

-- | A complete AsciiDoc document
data Document = Document
  { docMeta :: Meta
  , docBlocks :: [Block]
  } deriving (Show, Eq, Generic, Data, Typeable)

-- | Author information
data Author = Author
  { authorName :: Text
  , authorEmail :: Maybe Text
  } deriving (Show, Eq, Generic, Data, Typeable)

-- | Revision information
data Revision = Revision
  { revVersion :: Text
  , revDate :: Maybe Text
  , revRemark :: Maybe Text
  } deriving (Show, Eq, Generic, Data, Typeable)

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

attrNull :: Attr -> Bool
attrNull (Attr pos m) = null pos && Map.null m

-- | Nesting or section level
newtype Level = Level Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Programming or markup language identifier
newtype Language = Language Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Attribution for quotes
newtype Attribution = Attribution Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Alternative text for images
newtype AltText = AltText Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Width specification in pixels
newtype Width = Width Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Height specification in pixels
newtype Height = Height Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Footnote identifier
newtype FootnoteId = FootnoteId Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Attribute name
newtype AttributeName = AttributeName Text
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Source line callout
newtype Callout = Callout Int
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Source line with possible annotation
data SourceLine =
    SourceLine Text (Maybe Callout)
  deriving (Show, Eq, Ord, Generic, Data, Typeable)

-- | Block-level element with attributes
data Block = Block Attr (Maybe BlockTitle) BlockType
  deriving (Show, Eq, Generic, Data, Typeable)

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
  | HorizRule
  | PageBreak
  | ThematicBreak
  | Include FilePath (Maybe [Block])
  deriving (Show, Eq, Generic, Data, Typeable)

newtype BlockTitle = BlockTitle [Inline]
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Types of admonitions
data AdmonitionType
  = Note
  | Tip
  | Important
  | Caution
  | Warning
  deriving (Show, Eq, Generic, Data, Typeable)

-- | List types
data ListType
  = BulletList Level
  | OrderedList Level
  | CheckList
  | CalloutList
  deriving (Show, Eq, Generic, Data, Typeable)

-- | A list item
data ListItem = ListItem (Maybe CheckboxState) [Block]
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Checkbox state for checklists
data CheckboxState
  = Checked
  | Unchecked
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Column specification
data ColumnSpec = ColumnSpec
  { colHorizAlign :: Maybe HorizAlign
  , colVertAlign :: Maybe VertAlign
  , colWidth :: Maybe Int
  , colStyle :: Maybe CellStyle
  } deriving (Show, Eq, Generic, Data, Typeable)

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

-- | Table row
newtype TableRow = TableRow [TableCell]
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Table cell
data TableCell = TableCell
  { cellContent :: [Block]
  , cellHorizAlign :: Maybe HorizAlign
  , cellVertAlign :: Maybe VertAlign
  , cellColspan :: Int
  , cellRowspan :: Int
  } deriving (Show, Eq, Generic, Data, Typeable)

-- | Cell alignment
data HorizAlign
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Show, Eq, Generic, Data, Typeable)

data VertAlign
  = AlignTop
  | AlignMiddle
  | AlignBottom
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Inline element with attributes
data Inline = Inline Attr InlineType
  deriving (Show, Eq, Generic, Data, Typeable)

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

data MathType
  = AsciiMath
  | LaTeXMath
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Link types
data LinkType
  = URLLink
  | EmailLink
  deriving (Show, Eq, Generic, Data, Typeable)

-- | Link or image target
newtype Target = Target Text
  deriving (Show, Eq, Generic, Data, Typeable)
