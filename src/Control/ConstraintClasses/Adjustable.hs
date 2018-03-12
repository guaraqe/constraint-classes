module Control.ConstraintClasses.Adjustable
  (
  -- * Constraint Adjustable
    CAdjustable (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.KeyFunctor

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

class CKeyFunctor f => CAdjustable f where
  _update :: Dom f a => (a -> b -> a) -> f a -> [(CKey f, b)] -> f a
  _adjust :: Dom f a => (a -> a) -> CKey f -> f a -> f a
  _adjust f n v = _update (\a _ -> f a) v [(n,())]
  _replace :: Dom f a => CKey f -> a -> f a -> f a
  _replace n x = _adjust (const x) n

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

-- vector
instance CAdjustable VectorStorable.Vector where
  _update = VectorStorable.accum
  {-# INLINE _update #-}

instance CAdjustable VectorUnboxed.Vector where
  _update = VectorUnboxed.accum
  {-# INLINE _update #-}
