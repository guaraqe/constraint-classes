{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ConstraintClasses.Key
  (
  -- * Key classes
    CKey
  ) where

import Prelude hiding (lookup)

import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- vector
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Vector.Unboxed as VectorUnboxed

--------------------------------------------------------------------------------

-- | Equivalent to the @Key@ type family.
type family CKey (f :: * -> *)

type instance CKey [] = Int

type instance CKey Identity = ()
type instance CKey (Compose f g) = (CKey f, CKey g)
type instance CKey (Product f g) = Either (CKey f) (CKey g)
type instance CKey (Sum f g) = (CKey f, CKey g)

type instance CKey Vector.Vector = Int
type instance CKey VectorStorable.Vector = Int
type instance CKey VectorUnboxed.Vector = Int
