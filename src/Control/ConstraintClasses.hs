{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Control.ConstraintClasses
  (
  -- * Constraint
    Con (..)
  -- * Base classes
  , CFunctor (..)
  , CApplicative (..)
  , CMonad (..)
  , CAlternative (..)
  , CFoldable (..)
  , CTraversable (..)
  -- * Key classes
  , CKey (..)
  , CLookup (..)
  , (!?)
  , CIndexable (..)
  , (!)
  , CKeyed (..)
  , CZip (..)
  , CZipWithKey (..)
  , CFoldableWithKey (..)
  , CTraversableWithKey (..)
  , CAdjustable (..)
  -- * Distributive classes
  , CDistributive (..)
  -- * Representable classes
  , CRepresentable (..)
  ) where

import GHC.Exts (Constraint)

--------------------------------------------------------------------------------

-- | Type family representing the constraint for a given type. This class is
-- used in all the classes defined below.
type family Con (f :: * -> *) a :: Constraint

--------------------------------------------------------------------------------

-- | Equivalent to the @Functor@ class.
class CFunctor f where

  _fmap :: (Con f a, Con f b) => (a -> b) -> f a -> f b

--------------------------------------------------------------------------------

-- | Equivalent to the @Applicative@ class.
class CFunctor f => CApplicative f where

  _pure :: Con f a => a -> f a

  _liftA2 ::
    (Con f a, Con f b, Con f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _liftA3 ::
    (Con f a, Con f b, Con f c, Con f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _liftA4 ::
    (Con f a, Con f b, Con f c, Con f d, Con f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

--------------------------------------------------------------------------------

-- | Equivalent to the @Monad$ class.
class CApplicative f => CMonad f where

  _concatMap ::
    (Con f a, Con f b) =>
    (a -> f b) -> f a -> f b

--------------------------------------------------------------------------------

-- | Equivalent to the @Applicative@ class.
class CApplicative f => CAlternative f where

  _empty :: Con f a => f a

  _concat :: Con f a => f a -> f a -> f a

--------------------------------------------------------------------------------

-- | Equivalent to the @Foldable@ class.
class CFoldable f where
  {-# MINIMAL _foldMap | _foldr #-}

  _foldr  :: Con f a => (a -> b -> b) -> b -> f a -> b
  _foldr' :: Con f a => (a -> b -> b) -> b -> f a -> b
  _foldl  :: Con f b => (a -> b -> a) -> a -> f b -> a
  _foldl' :: Con f b => (a -> b -> a) -> a -> f b -> a

  _fold :: (Con f m, Monoid m) => f m -> m
  _fold = _foldMap id

  _foldMap :: (Con f a, Con f m, Monoid m) => (a -> m) -> f a -> m
  _foldMap f = _foldr (mappend . f) mempty
  {-# INLINE _foldMap #-}

  _toList :: Con f a => f a -> [a]
  _toList = _foldr (:) []

  _length :: Con f a => f a -> Int
  _length = _foldl (\c _ -> c+1) 0

  _mapM_ ::
    (Monad m, Con f a) =>
    (a -> m b) -> f a -> m ()
  _mapM_ f = _foldr ((>>) . f) (return ())

  _forM_ ::
    (Monad m, Con f a) =>
    f a -> (a -> m b) -> m ()
  _forM_ = flip _mapM_

--------------------------------------------------------------------------------

-- | Equivalent to the @Traversable@ class.
class (CFunctor t, CFoldable t) => CTraversable t where

  _traverse ::
    (Con t a, Con t b, Monad f) =>
    (a -> f b) -> t a -> f (t b)

  _sequence ::
    (Monad f, Con t a, Con t (f a)) =>
    t (f a) -> f (t a)
  _sequence = _traverse id

--------------------------------------------------------------------------------

-- | Equivalent to the @Zip@ class.
class CFunctor f => CZip f where

  _zipWith ::
    (Con f a, Con f b, Con f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _zipWith3 ::
    (Con f a, Con f b, Con f c, Con f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _zipWith4 ::
    (Con f a, Con f b, Con f c, Con f d, Con f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

--------------------------------------------------------------------------------

-- | Equivalent to the @Key@ type family.
type family CKey (f :: * -> *)

--------------------------------------------------------------------------------

-- | Equivalent to the @Lookup@ class.
class CLookup f where

  _lookup :: Con f a => CKey f -> f a -> Maybe a

(!?) :: (CLookup f, Con f a) => CKey f -> f a -> Maybe a
(!?) = _lookup

--------------------------------------------------------------------------------

-- | Equivalent to the @Indexable@ class.
class CLookup f => CIndexable f where

  _index :: Con f a => f a -> CKey f -> a

(!) :: (CIndexable f, Con f a) => f a -> CKey f -> a
(!) = _index

--------------------------------------------------------------------------------

-- | Equivalent to the @Keyed@ class.
class CFunctor f => CKeyed f where

   _imap ::
     (Con f a, Con f b) =>
     (CKey f -> a -> b) -> f a -> f b

--------------------------------------------------------------------------------

-- | Equivalent to the @Zip@ class.
class (CKeyed f, CZip f) => CZipWithKey f where

  _izipWith ::
    (Con f a, Con f b, Con f c) =>
    (CKey f -> a -> b -> c) -> f a -> f b -> f c

  _izipWith3 ::
    (Con f a, Con f b, Con f c, Con f d) =>
    (CKey f -> a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _izipWith4 ::
    (Con f a, Con f b, Con f c, Con f d, Con f e) =>
    (CKey f -> a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

--------------------------------------------------------------------------------

-- | Equivalent to the @FoldableWithKey@ class.
class CFoldable f => CFoldableWithKey f where

  _itoList :: Con f a => f a -> [(CKey f, a)]
  _ifoldMap :: (Monoid m, Con f a) => (CKey f -> a -> m) -> f a -> m
  _ifoldr :: Con f a => (CKey f -> a -> b -> b) -> b -> f a -> b
  _ifoldr' :: Con f a => (CKey f -> a -> b -> b) -> b -> f a -> b
  _ifoldl :: Con f b => (a -> CKey f -> b -> a) -> a -> f b -> a
  _ifoldl' :: Con f b => (a -> CKey f -> b -> a) -> a -> f b -> a

--------------------------------------------------------------------------------

-- | Equivalent to the @Adjustable@ class.
class CFunctor f => CAdjustable f where
  _adjust :: Con f a => CKey f -> f a -> f a
  _replace :: Con f a => CKey f -> a -> f a -> f a

--------------------------------------------------------------------------------

-- | Equivalent to the @Traversable@ class.
class (CKeyed t, CFoldableWithKey t, CTraversable t) =>
  CTraversableWithKey t where

  _itraverse ::
    (Con t a, Con t b, Monad f) =>
    (CKey t -> a -> f b) -> t a -> f (t b)

--------------------------------------------------------------------------------

-- | Equivalent to the @Distributive@ class.
class CFunctor t => CDistributive t where

  _collect ::
    (Con t b, Con t (f b), Functor f) =>
    (a -> t b) -> f a -> t (f b)
  _collect f = _distribute . fmap f

  _distribute ::
    (Con t (f a), Con t a, Functor f) =>
    f (t a) -> t (f a)
  _distribute = _collect id

--------------------------------------------------------------------------------

-- | Equivalent to the @Representable@ class. Thinking on the case where we do
-- not know at compile time the size of the container, it asks for an extra
-- parameter.
class (CDistributive f, CIndexable f) => CRepresentable f where
  _tabulate ::
    (Con f a, Con f b) =>
    (CKey f -> a) -> f b -> f a
