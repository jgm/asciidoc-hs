{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module AsciiDoc.Generic
  ( HasInlines(..)
  , HasBlocks(..)
  ) where

import GHC.Generics hiding (Meta)
import AsciiDoc.AST
import Control.Monad

class GHasInlines f where
  gfoldInlines :: Monoid m => (Inline -> m) -> f p -> m
  gmapInlines :: Monad m => (Inline -> m Inline) -> f p -> m (f p)

instance GHasInlines U1 where
  gfoldInlines _ _ = mempty
  gmapInlines _ _ = pure mempty

instance (GHasInlines f, GHasInlines g) => GHasInlines (f :*: g) where
  gfoldInlines f (x :*: y) = gfoldInlines f x <> gfoldInlines f y
  gmapInlines f (x :*: y) = liftM2 (:*:) (gmapInlines f x) (gmapInlines f y)

instance (GHasInlines f, GHasInlines g) => GHasInlines (f :+: g) where
  gfoldInlines f (L1 x) = gfoldInlines f x
  gfoldInlines f (R1 y) = gfoldInlines f y
  gmapInlines f (L1 x) = L1 <$> gmapInlines f x
  gmapInlines f (R1 y) = R1 <$> gmapInlines f y

instance GHasInlines f => GHasInlines (M1 i c f) where
  gfoldInlines f (M1 x) = gfoldInlines f x
  gmapInlines f (M1 x) = M1 <$> gmapInlines f x

class HasInlines a where
  foldInlines :: Monoid m => (Inline -> m) -> a -> m
  mapInlines :: Monad m => (Inline -> m Inline) -> a -> m a

  default foldInlines :: (Generic a, GHasInlines (Rep a), Monoid m)
                      => (Inline -> m) -> a -> m
  foldInlines f = gfoldInlines f . from

  default mapInlines :: (Generic a, GHasInlines (Rep a), Monad m)
                      => (Inline -> m Inline) -> a -> m a
  mapInlines f x = to <$> gmapInlines f (from x)

-- Field: delegate to HasInlines of the field type
instance HasInlines a => GHasInlines (K1 i a) where
  gfoldInlines f (K1 x) = foldInlines f x
  gmapInlines f (K1 x) = K1 <$> mapInlines f x

instance {-# OVERLAPPABLE #-} HasInlines a where
  foldInlines _ _ = mempty
  mapInlines _ x = pure x

instance (HasInlines a, Traversable t, Foldable t) => HasInlines (t a) where
  foldInlines f = foldMap (foldInlines f)
  mapInlines f = mapM (mapInlines f)

instance HasInlines Inline where
  foldInlines f i@(Inline _ ty) =
    f i <> foldInlines f ty
  mapInlines f (Inline attr ty) =
    (Inline attr <$> mapInlines f ty) >>= f

instance HasInlines Document
instance HasInlines Meta
instance HasInlines Author
instance HasInlines Block
instance HasInlines BlockType
instance HasInlines BlockTitle
instance HasInlines ListItem
instance HasInlines TableRow
instance HasInlines TableCell
instance HasInlines InlineType

class GHasBlocks f where
  gfoldBlocks :: Monoid m => (Block -> m) -> f p -> m
  gmapBlocks :: Monad m => (Block -> m Block) -> f p -> m (f p)

instance GHasBlocks U1 where
  gfoldBlocks _ _ = mempty
  gmapBlocks _ _ = pure mempty

instance (GHasBlocks f, GHasBlocks g) => GHasBlocks (f :*: g) where
  gfoldBlocks f (x :*: y) = gfoldBlocks f x <> gfoldBlocks f y
  gmapBlocks f (x :*: y) = liftM2 (:*:) (gmapBlocks f x) (gmapBlocks f y)

instance (GHasBlocks f, GHasBlocks g) => GHasBlocks (f :+: g) where
  gfoldBlocks f (L1 x) = gfoldBlocks f x
  gfoldBlocks f (R1 y) = gfoldBlocks f y
  gmapBlocks f (L1 x) = L1 <$> gmapBlocks f x
  gmapBlocks f (R1 y) = R1 <$> gmapBlocks f y

instance GHasBlocks f => GHasBlocks (M1 i c f) where
  gfoldBlocks f (M1 x) = gfoldBlocks f x
  gmapBlocks f (M1 x) = M1 <$> gmapBlocks f x

class HasBlocks a where
  foldBlocks :: Monoid m => (Block -> m) -> a -> m
  mapBlocks :: Monad m => (Block -> m Block) -> a -> m a

  default foldBlocks :: (Generic a, GHasBlocks (Rep a), Monoid m)
                      => (Block -> m) -> a -> m
  foldBlocks f = gfoldBlocks f . from

  default mapBlocks :: (Generic a, GHasBlocks (Rep a), Monad m)
                      => (Block -> m Block) -> a -> m a
  mapBlocks f x = to <$> gmapBlocks f (from x)

-- Field: delegate to HasBlocks of the field type
instance HasBlocks a => GHasBlocks (K1 i a) where
  gfoldBlocks f (K1 x) = foldBlocks f x
  gmapBlocks f (K1 x) = K1 <$> mapBlocks f x

instance {-# OVERLAPPABLE #-} HasBlocks a where
  foldBlocks _ _ = mempty
  mapBlocks _ x = pure x

instance (HasBlocks a, Traversable t, Foldable t) => HasBlocks (t a) where
  foldBlocks f = foldMap (foldBlocks f)
  mapBlocks f = mapM (mapBlocks f)

instance HasBlocks Block where
  foldBlocks f i@(Block _ _ ty) =
    f i <> foldBlocks f ty
  mapBlocks f (Block attr mbtit ty) =
    (Block attr mbtit <$> mapBlocks f ty) >>= f

instance HasBlocks Document
instance HasBlocks Meta
instance HasBlocks Inline
instance HasBlocks InlineType
instance HasBlocks ListItem
instance HasBlocks TableRow
instance HasBlocks TableCell
instance HasBlocks BlockType
 
