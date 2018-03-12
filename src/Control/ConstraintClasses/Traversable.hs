module Control.ConstraintClasses.Traversable
  (
  -- * Constraint Traversable
    CTraversable (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.FoldableFunctor

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

class CFoldableFunctor t => CTraversable t where
  _traverse ::
    (Dom t a, Dom t b, Applicative f) =>
    (a -> f b) -> t a -> f (t b)

  _sequenceA ::
    (Dom t a, Dom t (f a), Applicative f) =>
    t (f a) -> f (t a)
  _sequenceA = _traverse id

  _mapM ::
    (Dom t a, Dom t b, Monad f) =>
    (a -> f b) -> t a -> f (t b)
  _mapM = _traverse
  {-# INLINE _mapM #-}

  _sequence ::
    (Dom t a, Dom t (f a), Monad f) =>
    t (f a) -> f (t a)
  _sequence = _sequenceA
  {-# INLINE _sequence #-}


--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CTraversable [] where
  _traverse = traverse
  {-# INLINE _traverse #-}
  _sequenceA = sequenceA
  {-# INLINE _sequenceA #-}
  _mapM = mapM
  {-# INLINE _mapM #-}
  _sequence = sequence
  {-# INLINE _sequence #-}

-- vector

instance CTraversable Vector.Vector where
  _traverse f x =
    fmap Vector.fromList (traverse f (Vector.toList x))
  {-# INLINE _traverse #-}
  _mapM = Vector.mapM
  {-# INLINE _mapM #-}

instance CTraversable VectorStorable.Vector where
  _traverse f x =
    fmap VectorStorable.fromList (traverse f (VectorStorable.toList x))
  {-# INLINE _traverse #-}
  _mapM = VectorStorable.mapM
  {-# INLINE _mapM #-}

instance CTraversable VectorUnboxed.Vector where
  _traverse f x =
    fmap VectorUnboxed.fromList (traverse f (VectorUnboxed.toList x))
  {-# INLINE _traverse #-}
  _mapM = VectorUnboxed.mapM
  {-# INLINE _mapM #-}
