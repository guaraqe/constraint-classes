module Control.ConstraintClasses.KeyFoldableFunctor
  (
  -- * Constraint KeyFoldableFunctor
    CKeyFoldableFunctor
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.KeyFoldable
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

class (CKeyFunctor f, CKeyFoldable f) => CKeyFoldableFunctor f

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

-- vector

instance CKeyFoldableFunctor Vector.Vector
instance CKeyFoldableFunctor VectorStorable.Vector
instance CKeyFoldableFunctor VectorUnboxed.Vector
