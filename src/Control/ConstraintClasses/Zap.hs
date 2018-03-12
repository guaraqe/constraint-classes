{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.ConstraintClasses.Zap
  (
  -- * Constraint Zap
    CZap (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Zip

-- base
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- vector
import qualified Data.Vector as Vector

--------------------------------------------------------------------------------
-- CLASS
--------------------------------------------------------------------------------

class (DomClosed f, CZip f) => CZap f where
  _zap ::
    (Dom f a, Dom f b) =>
    f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CZap [] where
  _zap = zipWith ($)

-- vector

instance CZap Vector.Vector where
  _zap = _zipWith ($)
