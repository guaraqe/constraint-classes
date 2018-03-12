module Control.ConstraintClasses.Bind
  (
  -- * Constraint Bind
    CBind (..)
  , (>>=:)
  , (=<<:)
  , _concatMap
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

class CApply f => CBind f where
  _bind ::
    (Dom f a, Dom f b) =>
    (a -> f b) -> f a -> f b

infixl 1 >>=:
(>>=:) :: (CBind f, Dom f a, Dom f b) => f a -> (a -> f b) -> f b
(>>=:) = flip _bind
{-# INLINE (>>=:) #-}

infixr 1 =<<:
(=<<:) :: (CBind f, Dom f a, Dom f b) => (a -> f b) -> f a -> f b
(=<<:) = _bind
{-# INLINE (=<<:) #-}

_concatMap :: (CBind f, Dom f a, Dom f b) => (a -> f b) -> f a -> f b
_concatMap = _bind
{-# INLINE _concatMap #-}

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CBind [] where
  _bind = (=<<)

instance CBind Maybe where
  _bind = (=<<)

-- vector

instance CBind Vector.Vector where
  _bind = Vector.concatMap
  {-# INLINE _bind #-}

instance CBind VectorStorable.Vector where
  _bind = VectorStorable.concatMap
  {-# INLINE _bind #-}

instance CBind VectorUnboxed.Vector where
  _bind = VectorUnboxed.concatMap
  {-# INLINE _bind #-}
