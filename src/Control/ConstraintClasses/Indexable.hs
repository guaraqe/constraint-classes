module Control.ConstraintClasses.Indexable
  (
  -- * Constraint Indexable
    CIndexable (..)
  , (!)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.Lookup

import Data.Key hiding ((!))

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

-- | Equivalent to the @Indexable@ class.
class CLookup f => CIndexable f where
  _index :: Dom f a => f a -> CKey f -> a

(!) :: (CIndexable f, Dom f a) => f a -> CKey f -> a
(!) = _index
{-# INLINE [1] (!) #-}

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance (CIndexable f, CIndexable g) => CIndexable (Compose f g) where
  _index (Compose x) (i,j) = (x ! i) ! j
  {-# INLINE [1] _index #-}

-- vector

instance CIndexable Vector.Vector where
  _index = (Vector.!)
  {-# INLINE [1] _index #-}

instance CIndexable VectorStorable.Vector where
  _index = (VectorStorable.!)
  {-# INLINE [1] _index #-}

instance CIndexable VectorUnboxed.Vector where
  _index = (VectorUnboxed.!)
  {-# INLINE [1] _index #-}
