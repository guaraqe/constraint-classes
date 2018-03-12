module Control.ConstraintClasses.Foldable
  (
  -- * Constraint Foldable
    CFoldable (..)
  ) where

import Control.ConstraintClasses.Domain

import Data.List

-- base
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- vector
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Vector.Unboxed as VectorUnboxed

--------------------------------------------------------------------------------
-- CLASS
--------------------------------------------------------------------------------

class CFoldable f where
  {-# MINIMAL _foldMap | _foldr #-}

  _foldr  :: Dom f a => (a -> b -> b) -> b -> f a -> b
  _foldr' :: Dom f a => (a -> b -> b) -> b -> f a -> b
  _foldl  :: Dom f b => (a -> b -> a) -> a -> f b -> a
  _foldl' :: Dom f b => (a -> b -> a) -> a -> f b -> a

  _fold :: (Dom f m, Monoid m) => f m -> m
  _fold = _foldMap id
  {-# INLINE _fold #-}

  _foldMap :: (Dom f a, Monoid m) => (a -> m) -> f a -> m
  _foldMap f = _foldr (mappend . f) mempty
  {-# INLINE _foldMap #-}

  _toList :: Dom f a => f a -> [a]
  _toList = _foldr (:) []
  {-# INLINE _toList #-}

  _length :: Dom f a => f a -> Int
  _length = _foldl (\c _ -> c+1) 0
  {-# INLINE _length #-}

  _mapM_ ::
    (Monad m, Dom f a) =>
    (a -> m b) -> f a -> m ()
  _mapM_ f = _foldr ((>>) . f) (return ())
  {-# INLINE _mapM_ #-}

  _forM_ ::
    (Monad m, Dom f a) =>
    f a -> (a -> m b) -> m ()
  _forM_ = flip _mapM_
  {-# INLINE _forM_ #-}


--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CFoldable [] where
  _foldr = foldr
  {-# INLINE _foldr #-}
  _foldr' = foldr
  {-# INLINE _foldr' #-}
  _foldl = foldl
  {-# INLINE _foldl #-}
  _foldl' = foldl'
  {-# INLINE _foldl' #-}
  _foldMap = foldMap
  {-# INLINE _foldMap #-}
  _toList = id
  {-# INLINE _toList #-}
  _length = length
  {-# INLINE _length #-}
  _mapM_ = mapM_
  {-# INLINE _mapM_ #-}

-- vector

instance CFoldable Vector.Vector where
  _foldr = Vector.foldr
  {-# INLINE _foldr #-}
  _foldr' = Vector.foldr'
  {-# INLINE _foldr' #-}
  _foldl = Vector.foldl
  {-# INLINE _foldl #-}
  _foldl' = Vector.foldl'
  {-# INLINE _foldl' #-}
  _length = Vector.length
  {-# INLINE _length #-}
  _mapM_ = Vector.mapM_
  {-# INLINE _mapM_ #-}

instance CFoldable VectorStorable.Vector where
  _foldr = VectorStorable.foldr
  {-# INLINE _foldr #-}
  _foldr' = VectorStorable.foldr'
  {-# INLINE _foldr' #-}
  _foldl = VectorStorable.foldl
  {-# INLINE _foldl #-}
  _foldl' = VectorStorable.foldl'
  {-# INLINE _foldl' #-}
  _length = VectorStorable.length
  {-# INLINE _length #-}
  _mapM_ = VectorStorable.mapM_
  {-# INLINE _mapM_ #-}

instance CFoldable VectorUnboxed.Vector where
  _foldr = VectorUnboxed.foldr
  {-# INLINE _foldr #-}
  _foldr' = VectorUnboxed.foldr'
  {-# INLINE _foldr' #-}
  _foldl = VectorUnboxed.foldl
  {-# INLINE _foldl #-}
  _foldl' = VectorUnboxed.foldl'
  {-# INLINE _foldl' #-}
  _length = VectorUnboxed.length
  {-# INLINE _length #-}
  _mapM_ = VectorUnboxed.mapM_
  {-# INLINE _mapM_ #-}

