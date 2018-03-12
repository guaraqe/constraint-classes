{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintClasses.Distributive2
  ( CCDistributive (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Distributive
import Control.ConstraintClasses.Functor
import Control.ConstraintClasses.Monad

import Data.Functor.Compose

import Linear.V3

class CDistributive g => CCDistributive g where
  __distribute ::
    (Dom (Compose f g) a, Dom (Compose g f) a, CFunctor f) =>
    f (g a) -> g (f a)
  __distribute = __collect id
  {-# INLINE __distribute #-}

  __collect ::
    (Dom (Compose f g) b, Dom (Compose g f) b, Dom f a, CFunctor f) =>
    (a -> g b) -> f a -> g (f b)
  __collect f = __distribute . _fmap f
  {-# INLINE __collect #-}

  __distributeM ::
    (Dom (Compose m g) a, Dom (Compose g m) a, CMonad m) =>
    m (g a) -> g (m a)
  __distributeM = __distribute
  {-# INLINE __distributeM #-}

  __collectM ::
    (Dom (Compose m g) b, Dom (Compose g m) b, Dom m a, CMonad m) =>
    (a -> g b) -> m a -> g (m b)
  __collectM = __collect
  {-# INLINE __collectM #-}

--------------------------------------------------------------------------------

instance CCDistributive V3 where
  __distribute f =
    V3
      (_fmap (\(V3 x _ _) -> x) f)
      (_fmap (\(V3 _ y _) -> y) f)
      (_fmap (\(V3 _ _ z) -> z) f)
  {-# INLINE __distribute #-}
