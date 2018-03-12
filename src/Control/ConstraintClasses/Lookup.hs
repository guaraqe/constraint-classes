module Control.ConstraintClasses.Lookup
  (
  -- * Constraint Lookup
    CLookup (..)
  , (!?)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.Functor

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

class CFunctor f => CLookup f where
  _lookup :: Dom f a => CKey f -> f a -> Maybe a

(!?) :: (CLookup f, Dom f a) => f a -> CKey f -> Maybe a
(!?) = flip _lookup
{-# INLINE [1] (!?) #-}

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance (CLookup f, CLookup g) => CLookup (Compose f g) where
  _lookup (i,j) (Compose x) = _lookup i x >>= _lookup j
  {-# INLINE [1] _lookup #-}

-- vector
instance CLookup Vector.Vector where
  _lookup = flip (Vector.!?)
  {-# INLINE [1] _lookup #-}

instance CLookup VectorStorable.Vector where
  _lookup = flip (VectorStorable.!?)
  {-# INLINE [1] _lookup #-}

instance CLookup VectorUnboxed.Vector where
  _lookup = flip (VectorUnboxed.!?)
  {-# INLINE [1] _lookup #-}
