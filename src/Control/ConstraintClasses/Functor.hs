module Control.ConstraintClasses.Functor
  (
  -- * Constraint Functor
    CFunctor (..)
  , (<$>:)
  ) where

import Control.ConstraintClasses.Domain

-- base
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- linear
import Linear

-- vector
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Vector.Unboxed as VectorUnboxed

--------------------------------------------------------------------------------
-- CLASS
--------------------------------------------------------------------------------

class CFunctor f where
  _fmap :: (Dom f a, Dom f b) => (a -> b) -> f a -> f b

infixl 4 <$>:
(<$>:) :: (CFunctor f, Dom f a, Dom f b) => (a -> b) -> f a -> f b
(<$>:) = _fmap
{-# INLINE (<$>:) #-}

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CFunctor [] where
  _fmap = fmap

instance CFunctor Maybe where
  _fmap = fmap

instance (CFunctor f, CFunctor g) => CFunctor (Product f g) where
  _fmap f (Pair x y) = Pair (_fmap f x) (_fmap f y)

instance (CFunctor f, CFunctor g) => CFunctor (Sum f g) where
  _fmap f (InL x) = InL (_fmap f x)
  _fmap f (InR x) = InR (_fmap f x)

instance (CFunctor f, CFunctor g) => CFunctor (Compose f g) where
  _fmap f (Compose x) = Compose (_fmap (_fmap f) x)

-- linear

instance CFunctor V3 where
  _fmap = fmap

-- vector

instance CFunctor Vector.Vector where
  _fmap = Vector.map
  {-# INLINE _fmap #-}

instance CFunctor VectorStorable.Vector where
  _fmap = VectorStorable.map
  {-# INLINE _fmap #-}

instance CFunctor VectorUnboxed.Vector where
  _fmap = VectorUnboxed.map
  {-# INLINE _fmap #-}
