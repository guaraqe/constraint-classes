module Control.ConstraintClasses.MonadZero
  (
  -- * Constraint MonadZero
    CMonadZero
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Alternative
import Control.ConstraintClasses.Monad

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

class (CAlternative f, CMonad f) => CMonadZero f

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CMonadZero []
instance CMonadZero Maybe

-- vector
instance CMonadZero Vector.Vector
instance CMonadZero VectorStorable.Vector
instance CMonadZero VectorUnboxed.Vector
