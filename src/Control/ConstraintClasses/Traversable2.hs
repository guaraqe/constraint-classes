module Control.ConstraintClasses.Traversable2
  (
  -- * Double Constraint Traversable
    CCTraversable (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Traversable
import Control.ConstraintClasses.Applicative
import Control.ConstraintClasses.Monad

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

class CTraversable t => CCTraversable t where
  __traverse ::
    (Dom t a, Dom t b, CApplicative f) =>
    (a -> f b) -> t a -> f (t b)

  __sequenceA ::
    (Dom t a, Dom t (f a), CApplicative f) =>
    t (f a) -> f (t a)
  __sequenceA = __traverse id

  __mapM ::
    (Dom t a, Dom t b, CMonad f) =>
    (a -> f b) -> t a -> f (t b)
  __mapM = __traverse
  {-# INLINE __mapM #-}

  __sequence ::
    (Dom t a, Dom t (f a), CMonad f) =>
    t (f a) -> f (t a)
  __sequence = __sequenceA
  {-# INLINE __sequence #-}


--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

-- vector
