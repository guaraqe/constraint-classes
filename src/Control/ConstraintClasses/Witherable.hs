{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintClasses.Witherable
  (
    CWitherable (..)
  ) where

import Prelude hiding (filter)

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Traversable

--------------------------------------------------------------------------------

class CTraversable t => CWitherable t where

  _wither ::
    (Dom t a, Dom t b, Applicative f) =>
    (a -> f (Maybe b)) -> t a -> f (t b)

  _mapMaybe ::
    (Dom t a, Dom t b) =>
    (a -> Maybe b) -> t a -> t b

  _catMaybes ::
    (Dom t (Maybe a), Dom t a) =>
    t (Maybe a) -> t a

  _filterA ::
    (Applicative f, Dom t a, Dom f (t a), Dom f Bool) =>
    (a -> f Bool) -> t a -> f (t a)

  _filter ::
    Dom t a =>
    (a -> Bool) -> t a -> t a
