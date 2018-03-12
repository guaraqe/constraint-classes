module Control.ConstraintClasses.Alt
  (
  -- * Constraint Alt
    CAlt (..)
  , (<|>:)
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Functor

import Control.Applicative

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

class CFunctor f => CAlt f where
  _concat :: Dom f a => f a -> f a -> f a

infixl 3 <|>:
(<|>:) :: (CAlt f, Dom f a) => f a -> f a -> f a
(<|>:) = _concat
{-# INLINE (<|>:) #-}

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base

instance CAlt [] where
  _concat = (<|>)

instance CAlt Maybe where
  _concat = (<|>)


-- vector

instance CAlt Vector.Vector where
  _concat = (Vector.++)
  {-# INLINE _concat #-}

instance CAlt VectorStorable.Vector where
  _concat = (VectorStorable.++)
  {-# INLINE _concat #-}

instance CAlt VectorUnboxed.Vector where
  _concat = (VectorUnboxed.++)
  {-# INLINE _concat #-}

