{-# LANGUAGE DeriveFunctor #-}

module Data.List.Cut
  (
    CutListT'(..),
    CutListT(..),
    CutList,
    appCT,
  )
  where

import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Identity (Identity(..))

import Data.Monoid.Zero (MonoidRZero(..))

data CutListT' m a = CCons a (m (CutListT' m a)) | CNil | CCut
 deriving (Functor)

newtype CutListT m a = CutListT { unCutListT :: m (CutListT' m a) }
 deriving (Functor)

type CutList = CutListT Identity

appCT :: (Monad m) => CutListT m a -> CutListT m a -> CutListT m a
appCT (CutListT m) (CutListT n) = CutListT $ m >>= aux
 where
  aux (CCons a k) = return $ CCons a $ k >>= aux
  aux CNil        = n
  aux CCut        = return CNil

instance (Monad m) => Monad (CutListT m) where
  return a = CutListT $ return $ CCons a $ return CNil
  CutListT m >>= f = CutListT $ m >>= \x -> case x of
    CCons a m -> unCutListT $ f a `appCT` (CutListT m >>= f)
    CNil      -> return CNil
    CCut      -> return CCut

instance MonadTrans CutListT where
  lift m = CutListT $ liftM (\a -> CCons a $ return CNil) m
