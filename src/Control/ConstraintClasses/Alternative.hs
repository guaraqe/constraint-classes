module Control.ConstraintClasses.Alternative
  (
  -- * Constraint Alternative
    CAlternative
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Applicative
import Control.ConstraintClasses.Plus

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

class (CApplicative f, CPlus f) => CAlternative f

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CAlternative []
instance CAlternative Maybe

-- vector
instance CAlternative Vector.Vector
instance CAlternative VectorStorable.Vector
instance CAlternative VectorUnboxed.Vector
