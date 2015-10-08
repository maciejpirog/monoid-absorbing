{-# LANGUAGE DeriveFunctor, TypeFamilies #-}

{-|
Module      : Data.List.Cut
Copyright   : (c) 2015 Maciej Piróg
License     : MIT
Maintainer  : maciej.adam.pirog@gmail.com
Stability   : experimental

A @'ListT'@-like monad transformer equipped with the @'cut'@
operator known from Prolog.
-}
module Data.List.Cut
  (
    -- * The @CutListT@ transformer
    CutListT'(..),
    CutListT(..),
    CutList,
    -- * Additional functions
    (+<>+),
  )
  where

import Control.Monad (liftM, MonadPlus(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Identity (Identity(..))

import Data.Monoid.Zero (MonoidRZero(..))

data CutListT' m a = CCons a (m (CutListT' m a)) | CNil | CCut
 deriving (Functor)

-- | A monad transformer that behaves like the list transformer,
-- but it allows Prolog's cut operator.
newtype CutListT m a = CutListT { unCutListT :: m (CutListT' m a) }
 deriving (Functor)

type CutList = CutListT Identity

-- | Discard yet-uninspected choices.
cut :: (Monad m) => CutListT m ()
cut = CutListT $ return CCut

-- | Delimit the scope of cuts within the argument.
scope :: (Monad m) => CutListT m a -> CutListT m a
scope (CutListT m) = CutListT $ liftM aux m
 where
  aux (CCons a m) = CCons a (liftM aux m)
  aux CNil = CNil
  aux CCut = CNil

-- | Append two cut lists
(+<>+) :: (Monad m) => CutListT m a -> CutListT m a -> CutListT m a
CutListT m +<>+ CutListT n = CutListT $ m >>= aux
 where
  aux (CCons a k) = return $ CCons a $ k >>= aux
  aux CNil        = n
  aux CCut        = return CNil

instance (Monad m) => Monad (CutListT m) where
  return a = CutListT $ return $ CCons a $ return CNil
  CutListT m >>= f = CutListT $ m >>= \x -> case x of
    CCons a m -> unCutListT $ f a +<>+ (CutListT m >>= f)
    CNil      -> return CNil
    CCut      -> return CCut

instance MonadTrans CutListT where
  lift m = CutListT $ liftM (\a -> CCons a $ return CNil) m

instance (Monad m) => MonadPlus (CutListT m) where
  mzero = CutListT $ return CNil 
  mplus = (+<>+)
