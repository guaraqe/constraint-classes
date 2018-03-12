module Control.ConstraintClasses.MonadPlus
  (
  -- * Constraint MonadPlus
    CMonadPlus
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.MonadZero

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

class CMonadZero f => CMonadPlus f

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CMonadPlus []
instance CMonadPlus Maybe

-- vector
instance CMonadPlus Vector.Vector
instance CMonadPlus VectorStorable.Vector
instance CMonadPlus VectorUnboxed.Vector
