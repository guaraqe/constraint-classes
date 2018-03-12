module Control.ConstraintClasses.KeyFunctor
  (
  -- * Constraint KeyFunctor
    CKeyFunctor (..)
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

class CFunctor f => CKeyFunctor f where
   _imap ::
     (Dom f a, Dom f b) =>
     (CKey f -> a -> b) -> f a -> f b

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CKeyFunctor [] where
  _imap = mapWithKey
  {-# INLINE _imap #-}

-- vector

instance CKeyFunctor Vector.Vector where
  _imap = Vector.imap
  {-# INLINE _imap #-}

instance CKeyFunctor VectorStorable.Vector where
  _imap = VectorStorable.imap
  {-# INLINE _imap #-}

instance CKeyFunctor VectorUnboxed.Vector where
  _imap = VectorUnboxed.imap
  {-# INLINE _imap #-}
