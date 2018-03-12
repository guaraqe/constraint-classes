module Control.ConstraintClasses.Applicative
  (
  -- * Constraint Applicative
    CApplicative (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Apply

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

class CApply f => CApplicative f where
  _pure :: Dom f a => a -> f a

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CApplicative [] where
  _pure = pure

instance CApplicative Maybe where
  _pure = pure

-- vector
instance CApplicative Vector.Vector where
  _pure = Vector.singleton
  {-# INLINE _pure #-}

instance CApplicative VectorStorable.Vector where
  _pure = VectorStorable.singleton
  {-# INLINE _pure #-}

instance CApplicative VectorUnboxed.Vector where
  _pure = VectorUnboxed.singleton
  {-# INLINE _pure #-}
