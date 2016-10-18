{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Control.ConstraintClasses
  ( Con (..)
  , CFunctor (..)
  , CApplicative (..)
  , CMonad (..)
  , CAlternative (..)
  , CFoldable (..)
  , CTraversable (..)
  , CZippable (..)
  , CIndexed (..)
  , CIndexFunctor (..)
  , CIndexFoldable (..)
  ) where

import GHC.Exts (Constraint)

----------------------------------------------------------------------

type family Con (f :: * -> *) a :: Constraint

----------------------------------------------------------------------

class CFunctor f where

  _fmap :: (Con f a, Con f b) => (a -> b) -> f a -> f b

----------------------------------------------------------------------

class CFunctor f => CApplicative f where

  _pure :: (Con f a) => a -> f a

  _liftA2 ::
    (Con f a, Con f b, Con f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _liftA3 ::
    (Con f a, Con f b, Con f c, Con f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _liftA4 ::
    (Con f a, Con f b, Con f c, Con f d, Con f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

----------------------------------------------------------------------

class CApplicative f => CMonad f where

  _concatMap ::
    (Con f a, Con f b) =>
    (a -> f b) -> f a -> f b

----------------------------------------------------------------------

class CApplicative f => CAlternative f where

  _empty :: Con f a => f a

  _concat :: Con f a => f a -> f a -> f a

----------------------------------------------------------------------

class CFoldable f where
  {-# MINIMAL _foldMap | _foldr #-}

  _foldr  :: (Con f a) => (a -> b -> b) -> b -> f a -> b
  _foldr' :: (Con f a) => (a -> b -> b) -> b -> f a -> b
  _foldl  :: (Con f b) => (a -> b -> a) -> a -> f b -> a
  _foldl' :: (Con f b) => (a -> b -> a) -> a -> f b -> a

  _fold :: (Con f m, Monoid m) => f m -> m
  _fold = _foldMap id

  _foldMap :: (Con f a, Con f m, Monoid m) => (a -> m) -> f a -> m
  _foldMap f = _foldr (mappend . f) mempty
  {-# INLINE _foldMap #-}

  _length :: Con f a => f a -> Int
  _length = _foldl (\c _ -> c+1) 0

  _mapM_ ::
    (Monad m, Con f a) =>
    (a -> m b) -> f a -> m ()

  _forM_ ::
    (Monad m, Con f a) =>
    f a -> (a -> m b) -> m ()
  _forM_ = flip _mapM_

----------------------------------------------------------------------

class (CFunctor t, CFoldable t) => CTraversable t where

  _traverse ::
    (Con t a, Con t b, Monad f) =>
    (a -> f b) -> t a -> f (t b)

----------------------------------------------------------------------

class CFunctor f => CZippable f where

  _zipWith ::
    (Con f a, Con f b, Con f c) =>
    (a -> b -> c) -> f a -> f b -> f c

  _zipWith3 ::
    (Con f a, Con f b, Con f c, Con f d) =>
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d

  _zipWith4 ::
    (Con f a, Con f b, Con f c, Con f d, Con f e) =>
    (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e

----------------------------------------------------------------------

class CFunctor f => CIndexed f i | f -> i where

  (!) :: (Con f a) => f a -> i -> a

----------------------------------------------------------------------

class (CFunctor f, CIndexed f i) => CIndexFunctor f i | f -> i where

   _imap :: (Con f a, Con f b) => (i -> a -> b) -> f a -> f b

----------------------------------------------------------------------

class (CFoldable f, CIndexed f i) => CIndexFoldable f i | f -> i where

  _ifoldr :: Con f a => (i -> a -> b -> b) -> b -> f a -> b
  _ifoldr' :: Con f a => (i -> a -> b -> b) -> b -> f a -> b
  _ifoldl :: Con f b => (a -> i -> b -> a) -> a -> f b -> a
  _ifoldl' :: Con f b => (a -> i -> b -> a) -> a -> f b -> a
