{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintClasses.Distributive
  ( CDistributive (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Functor

import Data.Functor.Compose

import Linear.V3

class CFunctor g => CDistributive g where
  _distribute ::
    (Dom (Compose f g) a, Dom (Compose g f) a, Functor f) =>
    f (g a) -> g (f a)
  _distribute = _collect id
  {-# INLINE _distribute #-}

  _collect ::
    (Dom (Compose f g) b, Dom (Compose g f) b, Dom f a, Functor f) =>
    (a -> g b) -> f a -> g (f b)
  _collect f = _distribute . fmap f
  {-# INLINE _collect #-}

  _distributeM ::
    (Dom (Compose m g) a, Dom (Compose g m) a, Monad m) =>
    m (g a) -> g (m a)
  _distributeM = _distribute
  {-# INLINE _distributeM #-}

  _collectM ::
    (Dom (Compose m g) b, Dom (Compose g m) b, Dom m a, Monad m) =>
    (a -> g b) -> m a -> g (m b)
  _collectM = _collect
  {-# INLINE _collectM #-}

--------------------------------------------------------------------------------

instance CDistributive V3 where
  _distribute f =
    V3
      (fmap (\(V3 x _ _) -> x) f)
      (fmap (\(V3 _ y _) -> y) f)
      (fmap (\(V3 _ _ z) -> z) f)
  {-# INLINE _distribute #-}
