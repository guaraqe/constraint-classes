{-# LANGUAGE FlexibleContexts #-}

module Control.ConstraintClasses.Unzip
  (
  -- * Constraint Unzip
    CUnzip (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Functor

import Control.Arrow

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

class (DomCartesian f, CFunctor f) => CUnzip f where
  _unzip ::
    (Dom f a, Dom f b) =>
    f (a, b) -> (f a, f b)

  _unzipWith ::
    (Dom f a, Dom f b, Dom f c) =>
    (a -> (b,c)) -> f a -> (f b, f c)

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CUnzip [] where
  _unzip = _fmap fst &&& _fmap snd
  {-# INLINE _unzip #-}
  _unzipWith f = _fmap (fst . f) &&& _fmap (snd . f)
  {-# INLINE _unzipWith #-}

-- vector

instance CUnzip Vector.Vector where
  _unzip = _fmap fst &&& _fmap snd
  {-# INLINE _unzip #-}
  _unzipWith f = _fmap (fst . f) &&& _fmap (snd . f)
  {-# INLINE _unzipWith #-}

instance CUnzip VectorStorable.Vector where
  _unzip = _fmap fst &&& _fmap snd
  {-# INLINE _unzip #-}
  _unzipWith f = _fmap (fst . f) &&& _fmap (snd . f)
  {-# INLINE _unzipWith #-}

instance CUnzip VectorUnboxed.Vector where
  _unzip = _fmap fst &&& _fmap snd
  {-# INLINE _unzip #-}
  _unzipWith f = _fmap (fst . f) &&& _fmap (snd . f)
  {-# INLINE _unzipWith #-}

