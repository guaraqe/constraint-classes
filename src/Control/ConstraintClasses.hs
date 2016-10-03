{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}

module Control.ConstraintClasses
       ( CFunctor     (..)
       , CFoldable    (..) ) where

import GHC.Exts (Constraint)

----------------------------------------------------------------------

class CFunctor f where
  type CFun f a :: Constraint
  type CFun f a = ()

  _fmap :: (CFun f a, CFun f b) => (a -> b) -> f a -> f b

----------------------------------------------------------------------

class CFoldable f where

  type CFol f a :: Constraint
  type CFol f a = ()

  _foldr  :: (CFol f a) => (a -> b -> b) -> b -> f a -> b
  _foldr' :: (CFol f a) => (a -> b -> b) -> b -> f a -> b
  _foldl  :: (CFol f b) => (a -> b -> a) -> a -> f b -> a
  _foldl' :: (CFol f b) => (a -> b -> a) -> a -> f b -> a

  _fold :: (CFol f m, Monoid m) => f m -> m
  _fold = _foldMap id

  _foldMap :: (CFol f a, CFol f m, Monoid m) => (a -> m) -> f a -> m
  _foldMap f = _foldr (mappend . f) mempty

  _length :: CFol f a => f a -> Int
  _length = _foldl (\c _ -> c+1) 0

  _mapM :: (Monad m, CFol f a, CFol f b)
        => (a -> m b) -> f a -> m (f b)

  _forM :: (Monad m, CFol f a, CFol f b)
        => f a -> (a -> m b) -> m (f b)
  _forM = flip _mapM

  _mapM_ :: (Monad m, CFol f a)
         => (a -> m b) -> f a -> m ()

  _forM_ :: (Monad m, CFol f a)
         => f a -> (a -> m b) -> m ()
  _forM_ = flip _mapM_
