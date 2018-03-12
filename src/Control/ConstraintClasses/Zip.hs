{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.ConstraintClasses.Zip
  (
  -- * Constraint Zip
    CZip (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Functor

import Control.Applicative
import Data.Constraint
import Data.Proxy

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

class (DomCartesian f, CFunctor f) => CZip f where
  _zip ::
    forall a b .
    (Dom f a, Dom f b) =>
    f a -> f b -> f (a, b)
  _zip = _zipWith (,) \\ cartesian @(Dom f) (Proxy :: Proxy (a,b))

  _zipWith ::
    (Dom f a, Dom f b, Dom f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _zipWith3 ::
    (Dom f a, Dom f b, Dom f c, Dom f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _zipWith4 ::
    (Dom f a, Dom f b, Dom f c, Dom f d, Dom f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f a b c d =
  getZipList $ f <$> ZipList a <*> ZipList b <*> ZipList c <*> ZipList d

instance CZip [] where
  _zipWith = zipWith
  _zipWith3 = zipWith3
  _zipWith4 = zipWith4

-- vector

instance CZip Vector.Vector where
  _zipWith = Vector.zipWith
  {-# INLINE _zipWith #-}
  _zipWith3 = Vector.zipWith3
  {-# INLINE _zipWith3 #-}
  _zipWith4 = Vector.zipWith4
  {-# INLINE _zipWith4 #-}

instance CZip VectorStorable.Vector where
  _zipWith = VectorStorable.zipWith
  {-# INLINE _zipWith #-}
  _zipWith3 = VectorStorable.zipWith3
  {-# INLINE _zipWith3 #-}
  _zipWith4 = VectorStorable.zipWith4
  {-# INLINE _zipWith4 #-}

instance CZip VectorUnboxed.Vector where
  _zipWith = VectorUnboxed.zipWith
  {-# INLINE _zipWith #-}
  _zipWith3 = VectorUnboxed.zipWith3
  {-# INLINE _zipWith3 #-}
  _zipWith4 = VectorUnboxed.zipWith4
  {-# INLINE _zipWith4 #-}

