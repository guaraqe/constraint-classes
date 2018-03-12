module Control.ConstraintClasses.Monad
  (
  -- * Constraint Monad
    CMonad
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Applicative
import Control.ConstraintClasses.Bind

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

class (CApplicative f, CBind f) => CMonad f

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CMonad []
instance CMonad Maybe

-- vector
instance CMonad Vector.Vector
instance CMonad VectorStorable.Vector
instance CMonad VectorUnboxed.Vector
