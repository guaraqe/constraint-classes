module Control.ConstraintClasses.KeyTraversable
  (
  -- * Constraint KeyTraversable
    CKeyTraversable (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Key
import Control.ConstraintClasses.Traversable
import Control.ConstraintClasses.KeyFoldableFunctor

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

class (CKeyFoldableFunctor t, CTraversable t) =>
  CKeyTraversable t where
  _itraverse ::
    (Dom t a, Dom t b, Applicative f) =>
    (CKey t -> a -> f b) -> t a -> f (t b)
  _imapM ::
    (Dom t a, Dom t b, Monad f) =>
    (CKey t -> a -> f b) -> t a -> f (t b)

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

-- vector

instance CKeyTraversable Vector.Vector where
  _itraverse f v =
    Vector.fromList <$> traverseWithKey f (Vector.toList v)
  {-# INLINE _itraverse #-}
  _imapM f v =
     let rng = Vector.enumFromN 0 (Vector.length v)
     in Vector.zipWithM f rng v
  {-# INLINE _imapM #-}

instance CKeyTraversable VectorStorable.Vector where
  _itraverse f v =
    VectorStorable.fromList <$> traverseWithKey f (VectorStorable.toList v)
  {-# INLINE _itraverse #-}
  _imapM f v =
     let rng = VectorStorable.enumFromN 0 (VectorStorable.length v)
     in VectorStorable.zipWithM f rng v
  {-# INLINE _imapM #-}

instance CKeyTraversable VectorUnboxed.Vector where
  _itraverse f v =
    VectorUnboxed.fromList <$> traverseWithKey f (VectorUnboxed.toList v)
  {-# INLINE _itraverse #-}
  _imapM f v =
     let rng = VectorUnboxed.enumFromN 0 (VectorUnboxed.length v)
     in VectorUnboxed.zipWithM f rng v
  {-# INLINE _imapM #-}
