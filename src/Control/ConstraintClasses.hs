{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.ConstraintClasses
  (
  -- * Constraint
    Dom (..)
  -- * Base classes
  , CFunctor (..)
  , (<$>:)
  , CApply (..)
  , (<*>:)
  , CApplicative (..)
  , CMonad (..)
  , (>>=:)
  , (=<<:)
  , CAlt (..)
  , (<|>:)
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
  ) where

import GHC.Exts (Constraint)
import Data.Constraint
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Compose

--------------------------------------------------------------------------------

-- | The 'Dom' type family gives the domain of a given type constructor of kind
-- @* -> *@. This is meant to represent that @f@ is a function from a subset of
-- the the objects in @Hask@ to the objects of @Hask@.
type family Dom (f :: * -> *) a :: Constraint

class Any
instance Any

-- Data.Functor
type instance Dom Identity a = Any
type instance Dom (Product f g) a = (Dom f a, Dom g a)
type instance Dom (Sum f g) a = (Dom f a, Dom g a)
type instance Dom (Compose f g) a = (Dom g a, Dom f (g a))

-- transformers
type instance Dom (Constant a) b = Any

--------------------------------------------------------------------------------

class DomCartesian f where
  domCartesian :: (Dom f a, Dom f b) :- Dom f (a,b)

class DomCartesian f => DomClosed f where
  domClosed :: (Dom f a, Dom f b) :- Dom f (a -> b)

--------------------------------------------------------------------------------

-- | Equivalent to the @Functor@ class.
class CFunctor f where
  _fmap :: (Dom f a, Dom f b) => (a -> b) -> f a -> f b

infixl 4 <$>:
(<$>:) :: (CFunctor f, Dom f a, Dom f b) => (a -> b) -> f a -> f b
(<$>:) = _fmap

instance CFunctor (Constant a) where
  _fmap = fmap

instance CFunctor Identity where
  _fmap = fmap

instance (CFunctor f, CFunctor g) => CFunctor (Product f g) where
  _fmap f (Pair l r) = Pair (_fmap f l) (_fmap f r)

instance (CFunctor f, CFunctor g) => CFunctor (Sum f g) where
  _fmap f (InL x) = InL (_fmap f x)
  _fmap f (InR x) = InR (_fmap f x)

instance (CFunctor f, CFunctor g) => CFunctor (Compose f g) where
  _fmap f (Compose x) = Compose (_fmap (_fmap f) x)

--------------------------------------------------------------------------------

-- | Equivalent to the @Applicative@ class.
class CFunctor f => CApply f where
  _zipA ::
    forall a b.
    (DomCartesian f, Dom f a, Dom f b) =>
    f a -> f b -> f (a, b)
  _zipA x y = _liftA2 (,) x y \\ domCartesian @f @a @b

  _liftA2 ::
    (Dom f a, Dom f b, Dom f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _liftA3 ::
    (Dom f a, Dom f b, Dom f c, Dom f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _liftA4 ::
    (Dom f a, Dom f b, Dom f c, Dom f d, Dom f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

  _ap ::
    (DomClosed f, Dom f a, Dom f b) =>
    f (a -> b) -> f a -> f b

infixl 4 <*>:
(<*>:) :: (CApply f, DomClosed f, Dom f a, Dom f b) => f (a -> b) -> f a -> f b
(<*>:) = _ap

class CApply f => CApplicative f where
  _pure :: Dom f a => a -> f a

--------------------------------------------------------------------------------

-- | Equivalent to the @Monad$ class.
class CApplicative f => CMonad f where
  _concatMap ::
    (Dom f a, Dom f b) =>
    (a -> f b) -> f a -> f b

infixl 1 >>=:
(>>=:) :: (CMonad f, Dom f a, Dom f b) => f a -> (a -> f b) -> f b
(>>=:) = flip _concatMap

infixr 1 =<<:
(=<<:) :: (CMonad f, Dom f a, Dom f b) => (a -> f b) -> f a -> f b
(=<<:) = _concatMap

--------------------------------------------------------------------------------

-- | Equivalent to the @Applicative@ class.
class CApplicative f => CAlt f where
  _concat :: Dom f a => f a -> f a -> f a

infixl 3 <|>:
(<|>:) :: (CAlt f, Dom f a) => f a -> f a -> f a
(<|>:) = _concat

class CAlt f => CAlternative f where
  _empty :: Dom f a => f a

--------------------------------------------------------------------------------

-- | Equivalent to the @Foldable@ class.
class CFoldable f where
  {-# MINIMAL _foldMap | _foldr #-}

  _foldr  :: Dom f a => (a -> b -> b) -> b -> f a -> b
  _foldr' :: Dom f a => (a -> b -> b) -> b -> f a -> b
  _foldl  :: Dom f b => (a -> b -> a) -> a -> f b -> a
  _foldl' :: Dom f b => (a -> b -> a) -> a -> f b -> a

  _fold :: (Dom f m, Monoid m) => f m -> m
  _fold = _foldMap id

  _foldMap :: (Dom f a, Monoid m) => (a -> m) -> f a -> m
  _foldMap f = _foldr (mappend . f) mempty
  {-# INLINE _foldMap #-}

  _toList :: Dom f a => f a -> [a]
  _toList = _foldr (:) []

  _length :: Dom f a => f a -> Int
  _length = _foldl (\c _ -> c+1) 0

  _mapM_ ::
    (Monad m, Dom f a) =>
    (a -> m b) -> f a -> m ()
  _mapM_ f = _foldr ((>>) . f) (return ())

  _forM_ ::
    (Monad m, Dom f a) =>
    f a -> (a -> m b) -> m ()
  _forM_ = flip _mapM_

--------------------------------------------------------------------------------

-- | Equivalent to the @Traversable@ class.
class (CFunctor t, CFoldable t) => CTraversable t where
  _traverse ::
    (Dom t a, Dom t b, Monad f) =>
    (a -> f b) -> t a -> f (t b)

  _sequence ::
    (Monad f, Dom t a, Dom t (f a)) =>
    t (f a) -> f (t a)
  _sequence = _traverse id

--------------------------------------------------------------------------------

-- | Equivalent to the @Zip@ class.
class CFunctor f => CZip f where
  _zip ::
    (DomCartesian f, Dom f a, Dom f b) =>
    f a -> f b -> f (a, b)

  _zipWith ::
    (Dom f a, Dom f b, Dom f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _zipWith3 ::
    (Dom f a, Dom f b, Dom f c, Dom f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _zipWith4 ::
    (Dom f a, Dom f b, Dom f c, Dom f d, Dom f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

  _zipAp ::
    (DomClosed f, Dom f a, Dom f b) =>
    f (a -> b) -> f a -> f b

--------------------------------------------------------------------------------

-- | Equivalent to the @Key@ type family.
type family CKey (f :: * -> *)

--------------------------------------------------------------------------------

-- | Equivalent to the @Lookup@ class.
class CLookup f where
  _lookup :: Dom f a => CKey f -> f a -> Maybe a

(!?) :: (CLookup f, Dom f a) => CKey f -> f a -> Maybe a
(!?) = _lookup

--------------------------------------------------------------------------------

-- | Equivalent to the @Indexable@ class.
class CLookup f => CIndexable f where
  _index :: Dom f a => f a -> CKey f -> a

(!) :: (CIndexable f, Dom f a) => f a -> CKey f -> a
(!) = _index

--------------------------------------------------------------------------------

-- | Equivalent to the @Keyed@ class.
class CFunctor f => CKeyed f where
   _imap ::
     (Dom f a, Dom f b) =>
     (CKey f -> a -> b) -> f a -> f b

--------------------------------------------------------------------------------

-- | Equivalent to the @Zip@ class.
class (CKeyed f, CZip f) => CZipWithKey f where
  _izipWith ::
    (Dom f a, Dom f b, Dom f c) =>
    (CKey f -> a -> b -> c) -> f a -> f b -> f c

  _izipWith3 ::
    (Dom f a, Dom f b, Dom f c, Dom f d) =>
    (CKey f -> a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _izipWith4 ::
    (Dom f a, Dom f b, Dom f c, Dom f d, Dom f e) =>
    (CKey f -> a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

--------------------------------------------------------------------------------

-- | Equivalent to the @FoldableWithKey@ class.
class CFoldable f => CFoldableWithKey f where
  _itoList :: Dom f a => f a -> [(CKey f, a)]
  _ifoldMap :: (Monoid m, Dom f a) => (CKey f -> a -> m) -> f a -> m
  _ifoldr :: Dom f a => (CKey f -> a -> b -> b) -> b -> f a -> b
  _ifoldr' :: Dom f a => (CKey f -> a -> b -> b) -> b -> f a -> b
  _ifoldl :: Dom f b => (a -> CKey f -> b -> a) -> a -> f b -> a
  _ifoldl' :: Dom f b => (a -> CKey f -> b -> a) -> a -> f b -> a

--------------------------------------------------------------------------------

-- | Equivalent to the @Adjustable@ class.
class CFunctor f => CAdjustable f where
  _update :: Dom f a => (a -> b -> a) -> f a -> [(CKey f, b)] -> f a
  _adjust :: Dom f a => (a -> a) -> CKey f -> f a -> f a
  _adjust f n v = _update (\a _ -> f a) v [(n,())]
  _replace :: Dom f a => CKey f -> a -> f a -> f a
  _replace n x = _adjust (const x) n

--------------------------------------------------------------------------------

-- | Equivalent to the @Traversable@ class.
class (CKeyed t, CFoldableWithKey t, CTraversable t) =>
  CTraversableWithKey t where
  _itraverse ::
    (Dom t a, Dom t b, Monad f) =>
    (CKey t -> a -> f b) -> t a -> f (t b)
