{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.ConstraintClasses.Domain
  (
  -- * Domain constraints and types
    Dom
  , Cartesian (..)
  , DomCartesian
  , Closed (..)
  , DomClosed
  -- * Accessory classes
  , Anything
  , (::*::)
  , (::.::)
  ) where

import Prelude hiding ((.))

import GHC.Exts (Constraint)
import Data.Constraint
import Data.Proxy

import Control.Category

import Data.Functor.Constant

import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

-- linear
import Linear.V3

-- containers
import qualified Data.IntMap.Strict as IntMapStrict
import qualified Data.IntMap as IntMapLazy
import qualified Data.Map.Strict as MapStrict
import qualified Data.Map as MapLazy
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

-- vector
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as VectorStorable
import qualified Data.Vector.Unboxed as VectorUnboxed

import Foreign.Storable.Tuple ()

--------------------------------------------------------------------------------

-- | The 'Dom' type family gives the domain of a given type constructor of kind
-- @* -> *@. This is meant to represent that @f@ is a function from a subset of
-- the the objects in @Hask@ to the objects of @Hask@.
type family Dom (f :: * -> *) :: * -> Constraint

--------------------------------------------------------------------------------

class (k1 a, k2 a) => (k1 ::*:: k2) a
instance (k1 a, k2 a) => (k1 ::*:: k2) a

instance Class (k1 a, k2 a) ((k1 ::*:: k2) a) where cls = Sub Dict
instance (k1 a, k2 a) :=> ((k1 ::*:: k2) a) where ins = Sub Dict

class k (g a) => (k ::.:: g) a
instance k (g a) => (k ::.:: g) a

instance Class (k (g a)) ((k ::.:: g) a) where cls = Sub Dict
instance (k (g a)) :=> ((k ::.:: g) a) where ins = Sub Dict

type instance Dom (Product f g) = Dom f ::*:: Dom g

type instance Dom (Compose f g) = (Dom f ::.:: g) ::*:: Dom g

type instance Dom (Sum f g) = Dom f ::*:: Dom g

--------------------------------------------------------------------------------

class Anything a
instance Anything a

type instance Dom Identity = Anything

--------------------------------------------------------------------------------

-- transformers
type instance Dom (Constant a) = Anything

-- base
type instance Dom [] = Anything
type instance Dom Maybe = Anything

-- linear
type instance Dom V3 = Anything

-- containers
type instance Dom IntMapStrict.IntMap = Anything
type instance Dom IntMapLazy.IntMap = Anything
type instance Dom (MapStrict.Map k) = Anything
type instance Dom (MapLazy.Map k) = Anything
type instance Dom Set.Set = Anything
type instance Dom Seq.Seq = Anything

-- vector
type instance Dom Vector.Vector = Anything
type instance Dom VectorStorable.Vector = VectorStorable.Storable
type instance Dom VectorUnboxed.Vector = VectorUnboxed.Unbox

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

class Cartesian (k :: * -> Constraint) where
  cartesian :: Proxy (a,b) -> (k a, k b) :- k (a, b)

type DomCartesian f = Cartesian (Dom f)

instance (Cartesian k1, Cartesian k2) => Cartesian (k1 ::*:: k2) where
  cartesian p@(Proxy :: Proxy (a,b)) =
    ins .
    (cartesian @k1 p *** cartesian @k2 p) .
    Sub Dict .
    (cls *** cls)

-- instances
instance Cartesian Anything where
  cartesian Proxy = Sub Dict

instance Cartesian Ord where
  cartesian Proxy = Sub Dict

instance Cartesian Eq where
  cartesian Proxy = Sub Dict

instance Cartesian VectorUnboxed.Unbox where
  cartesian Proxy = Sub Dict

instance Cartesian VectorStorable.Storable where
  cartesian Proxy = Sub Dict

--------------------------------------------------------------------------------

class Cartesian k => Closed (k :: * -> Constraint) where
  closed :: (k a, k b) :- k (a -> b)

type DomClosed f = Closed (Dom f)

instance Closed Anything where
  closed = Sub Dict
