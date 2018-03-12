module Control.ConstraintClasses.KeyZip
  (
  -- * Constraint KeyZip
    CKeyZip (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.KeyFunctor
import Control.ConstraintClasses.Zip

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

-- | Equivalent to the @Zip@ class.
class (CKeyFunctor f, CZip f) => CKeyZip f where
  _izipWith ::
    (Dom f a, Dom f b, Dom f c) =>
    (CKey f -> a -> b -> c) -> f a -> f b -> f c

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

-- vector
instance CKeyZip VectorStorable.Vector where
  _izipWith = VectorStorable.izipWith
  {-# INLINE _izipWith #-}

instance CKeyZip VectorUnboxed.Vector where
  _izipWith = VectorUnboxed.izipWith
  {-# INLINE _izipWith #-}
