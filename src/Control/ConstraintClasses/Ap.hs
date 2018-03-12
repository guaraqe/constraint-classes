{-# LANGUAGE FlexibleContexts #-}

module Control.ConstraintClasses.Ap
  (
  -- * Constraint Apply
    CAp (..)
  , (<*>:)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Apply

-- base
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- vector
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- CLASS
--------------------------------------------------------------------------------

class (DomClosed f, CApply f) => CAp f where
  _ap ::
    (Dom f a, Dom f b) =>
    f (a -> b) -> f a -> f b

infixl 4 <*>:
(<*>:) :: (CAp f, Dom f a, Dom f b) => f (a -> b) -> f a -> f b
(<*>:) = _ap
{-# INLINE (<*>:) #-}

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CAp [] where
  _ap = (<*>)

instance CAp Maybe where
  _ap = (<*>)

-- vector
instance CAp Vector.Vector where
  _ap = (<*>)


