{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.ConstraintClasses.Apply
  (
  -- * Constraint Apply
    CApply (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Functor

import Data.Proxy
import Data.Constraint

import Control.Applicative

-- base
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- vector
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Vector.Unboxed as VectorUnboxed
import qualified Data.Vector.Generic as VectorGeneric

--------------------------------------------------------------------------------
-- CLASS
--------------------------------------------------------------------------------

class (DomCartesian f, CFunctor f) => CApply f where
  _zipA ::
    forall a b .
    (Dom f a, Dom f b) =>
    f a -> f b -> f (a, b)
  _zipA x y = _liftA2 (,) x y  \\
    cartesian @(Dom f) (Proxy :: Proxy (a,b))
  {-# INLINE _zipA #-}

  _liftA2 ::
    (Dom f a, Dom f b, Dom f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _liftA3 ::
    (Dom f a, Dom f b, Dom f c, Dom f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _liftA4 ::
    (Dom f a, Dom f b, Dom f c, Dom f d, Dom f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

liftA4 ::
  Applicative f =>
  (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

instance CApply [] where
  _liftA2 = liftA2
  _liftA3 = liftA3
  _liftA4 = liftA4

instance CApply Maybe where
  _liftA2 = liftA2
  _liftA3 = liftA3
  _liftA4 = liftA4

instance (CApply f, CApply g) => CApply (Product f g) where
  _liftA2 f (Pair x1 y1) (Pair x2 y2) =
    Pair (_liftA2 f x1 x2) (_liftA2 f y1 y2)

  _liftA3 f (Pair x1 y1) (Pair x2 y2) (Pair x3 y3)=
    Pair (_liftA3 f x1 x2 x3) (_liftA3 f y1 y2 y3)

  _liftA4 f (Pair x1 y1) (Pair x2 y2) (Pair x3 y3) (Pair x4 y4) =
    Pair (_liftA4 f x1 x2 x3 x4) (_liftA4 f y1 y2 y3 y4)

{-
instance (CApply f, CApply g) => CApply (Compose f g) where
  _liftA2 f (Compose x) (Compose y) =
    Compose $ _liftA2 (_liftA2 f) x y

  _liftA3 f (Compose x) (Compose y) (Compose z) =
    Compose $ _liftA3 (_liftA3 f) x y z

  _liftA4 f (Compose x) (Compose y) (Compose z) (Compose w) =
    Compose $ _liftA4 (_liftA4 f) x y z w
-}

-- vector

outer2 ::
  ( VectorGeneric.Vector v a
  , VectorGeneric.Vector v b
  , VectorGeneric.Vector v c ) =>
  (a -> b -> c) -> v a -> v b -> v c
outer2 f v1 v2 =
  VectorGeneric.concatMap (\x -> VectorGeneric.map (f x) v2) v1
{-# INLINE outer2 #-}

outer3 ::
  ( VectorGeneric.Vector v a
  , VectorGeneric.Vector v b
  , VectorGeneric.Vector v c
  , VectorGeneric.Vector v d ) =>
  (a -> b -> c -> d) -> v a -> v b -> v c -> v d
outer3 f v1 v2 v3 =
  VectorGeneric.concatMap (\x -> outer2 (f x) v2 v3) v1
{-# INLINE outer3 #-}

outer4 ::
  ( VectorGeneric.Vector v a
  , VectorGeneric.Vector v b
  , VectorGeneric.Vector v c
  , VectorGeneric.Vector v d
  , VectorGeneric.Vector v e ) =>
  (a -> b -> c -> d -> e) -> v a -> v b -> v c -> v d -> v e
outer4 f v1 v2 v3 v4 =
  VectorGeneric.concatMap (\x -> outer3 (f x) v2 v3 v4) v1
{-# INLINE outer4 #-}

instance CApply Vector.Vector where
  _liftA2 = outer2
  {-# INLINE _liftA2 #-}
  _liftA3 = outer3
  {-# INLINE _liftA3 #-}
  _liftA4 = outer4
  {-# INLINE _liftA4 #-}

instance CApply VectorStorable.Vector where
  _liftA2 = outer2
  {-# INLINE _liftA2 #-}
  _liftA3 = outer3
  {-# INLINE _liftA3 #-}
  _liftA4 = outer4
  {-# INLINE _liftA4 #-}

instance CApply VectorUnboxed.Vector where
  _liftA2 = outer2
  {-# INLINE _liftA2 #-}
  _liftA3 = outer3
  {-# INLINE _liftA3 #-}
  _liftA4 = outer4
  {-# INLINE _liftA4 #-}
