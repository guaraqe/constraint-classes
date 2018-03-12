module Control.ConstraintClasses.Plus
  (
  -- * Constraint Plus
    CPlus (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Alt

import Control.Applicative

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

class CAlt f => CPlus f where
  _empty :: Dom f a => f a

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CPlus [] where
  _empty = empty

instance CPlus Maybe where
  _empty = empty

-- vector

instance CPlus Vector.Vector where
  _empty = Vector.empty
  {-# INLINE _empty #-}

instance CPlus VectorStorable.Vector where
  _empty = VectorStorable.empty
  {-# INLINE _empty #-}

instance CPlus VectorUnboxed.Vector where
  _empty = VectorUnboxed.empty
  {-# INLINE _empty #-}
