module Control.ConstraintClasses.FoldableFunctor
  (
  -- * Constraint FoldableFunctor
    CFoldableFunctor
  ) where

import Control.ConstraintClasses.Domain
import Control.ConstraintClasses.Functor
import Control.ConstraintClasses.Foldable

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

class (CFunctor f, CFoldable f) => CFoldableFunctor f

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- base
instance CFoldableFunctor []

-- vector
instance CFoldableFunctor Vector.Vector
instance CFoldableFunctor VectorStorable.Vector
instance CFoldableFunctor VectorUnboxed.Vector
