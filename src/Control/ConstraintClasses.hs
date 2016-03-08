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

  cmap :: (CFun f a, CFun f b) => (a -> b) -> f a -> f b

----------------------------------------------------------------------

class CFoldable f where

  type CFol f a :: Constraint
  type CFol f a = ()

  cfoldr  :: (CFol f a) => (a -> b -> b) -> b -> f a -> b
  cfoldr' :: (CFol f a) => (a -> b -> b) -> b -> f a -> b
  cfoldl  :: (CFol f b) => (a -> b -> a) -> a -> f b -> a
  cfoldl' :: (CFol f b) => (a -> b -> a) -> a -> f b -> a

  cfold :: (CFol f m, Monoid m) => f m -> m
  cfold = cfoldMap id

  cfoldMap :: (CFol f a, CFol f m, Monoid m) => (a -> m) -> f a -> m
  cfoldMap f = cfoldr (mappend . f) mempty

  clength :: CFol f a => f a -> Int
  clength = cfoldl (\c _ -> c+1) 0

  cmapM :: (Monad m, CFol f a, CFol f b)
        => (a -> m b) -> f a -> m (f b)

  cforM :: (Monad m, CFol f a, CFol f b)
        => f a -> (a -> m b) -> m (f b)
  cforM = flip cmapM

  cmapM_ :: (Monad m, CFol f a)
         => (a -> m b) -> f a -> m ()

  cforM_ :: (Monad m, CFol f a)
         => f a -> (a -> m b) -> m ()
  cforM_ = flip cmapM_
