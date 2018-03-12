module Control.ConstraintClasses.KeyFoldable
  (
  -- * Constraint KeyFoldable
    CKeyFoldable (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.Foldable

import Data.Key

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

class CFoldable f => CKeyFoldable f where
  _itoList :: Dom f a => f a -> [(CKey f, a)]
  _ifoldMap :: (Monoid m, Dom f a) => (CKey f -> a -> m) -> f a -> m
  _ifoldr :: Dom f a => (CKey f -> a -> b -> b) -> b -> f a -> b
  _ifoldr' :: Dom f a => (CKey f -> a -> b -> b) -> b -> f a -> b
  _ifoldl :: Dom f b => (a -> CKey f -> b -> a) -> a -> f b -> a
  _ifoldl' :: Dom f b => (a -> CKey f -> b -> a) -> a -> f b -> a

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

-- vector

instance CKeyFoldable Vector.Vector where
  _ifoldr = Vector.ifoldr
  {-# INLINE _ifoldr #-}
  _ifoldr' = Vector.ifoldr'
  {-# INLINE _ifoldr' #-}
  _ifoldl = Vector.ifoldl'
  {-# INLINE _ifoldl #-}
  _ifoldl' = Vector.ifoldl'
  {-# INLINE _ifoldl' #-}

instance CKeyFoldable VectorStorable.Vector where
  _ifoldr = VectorStorable.ifoldr
  {-# INLINE _ifoldr #-}
  _ifoldr' = VectorStorable.ifoldr'
  {-# INLINE _ifoldr' #-}
  _ifoldl = VectorStorable.ifoldl'
  {-# INLINE _ifoldl #-}
  _ifoldl' = VectorStorable.ifoldl'
  {-# INLINE _ifoldl' #-}

instance CKeyFoldable VectorUnboxed.Vector where
  _ifoldr = VectorUnboxed.ifoldr
  {-# INLINE _ifoldr #-}
  _ifoldr' = VectorUnboxed.ifoldr'
  {-# INLINE _ifoldr' #-}
  _ifoldl = VectorUnboxed.ifoldl'
  {-# INLINE _ifoldl #-}
  _ifoldl' = VectorUnboxed.ifoldl'
  {-# INLINE _ifoldl' #-}
