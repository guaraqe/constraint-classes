{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintClasses.Witherable2
  (
    CCWitherable (..)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Witherable
import Control.ConstraintClasses.Applicative

--------------------------------------------------------------------------------

class CWitherable t => CCWitherable t where

  __wither ::
    (Dom t a, Dom t b, CApplicative f) =>
    (a -> f (Maybe b)) -> t a -> f (t b)

  __mapMaybe ::
    (Dom t a, Dom t b) =>
    (a -> Maybe b) -> t a -> t b

  __catMaybes ::
    (Dom t (Maybe a), Dom t a) =>
    t (Maybe a) -> t a

  __filterA ::
    (CApplicative f, Dom t a, Dom f (t a), Dom f Bool) =>
    (a -> f Bool) -> t a -> f (t a)

  __filter ::
    Dom t a =>
    (a -> Bool) -> t a -> t a
