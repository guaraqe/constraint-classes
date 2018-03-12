module Control.ConstraintClasses.Eq1
  (
  -- * Constraint Eq1
    CEq1 (..)
  , _eq1
  ) where

import Control.ConstraintClasses.Domain

import Data.Functor.Classes

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

class CEq1 f where
  _liftEq :: (Dom f a, Dom f b) => (a -> b -> Bool) -> f a -> f b -> Bool

_eq1 :: (CEq1 f, Eq a, Dom f a) => f a -> f a -> Bool
_eq1 = _liftEq (==)

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CEq1 [] where
  _liftEq = liftEq

instance (CEq1 f, CEq1 g) => CEq1 (Compose f g) where
  _liftEq eq (Compose x) (Compose y) = _liftEq (_liftEq eq) x y

-- vector
instance CEq1 Vector.Vector where
  _liftEq eq xs ys = _liftEq eq (Vector.toList xs) (Vector.toList ys)

instance CEq1 VectorStorable.Vector where
  _liftEq eq xs ys = _liftEq eq (VectorStorable.toList xs) (VectorStorable.toList ys)

instance CEq1 VectorUnboxed.Vector where
  _liftEq eq xs ys = _liftEq eq (VectorUnboxed.toList xs) (VectorUnboxed.toList ys)
