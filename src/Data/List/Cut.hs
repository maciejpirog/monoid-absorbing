{-# LANGUAGE DeriveFunctor #-}

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
    retract,
    -- * The @CutList@ monad
    CutList,
    toList,
    fromList,
    -- * Control functions
    cut,
    cutFail,
    scope,

    -- * Examples

    -- ** @takeWhile@ using lists with cut

    -- $takeWhile
  )
  where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (liftM, MonadPlus(..), ap)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Identity (Identity(..))
import Data.Foldable (Foldable (..))
import Data.Traversable (Traversable(..))
import Data.Monoid (Monoid(..))

import Data.Monoid.Zero (MonoidRZero(..))

data CutListT' m a = CCons a (m (CutListT' m a)) | CNil | CCut
 deriving (Functor)

instance (Foldable m) => Foldable (CutListT' m) where
  foldMap f (CCons a m) = f a `mappend` foldMap (foldMap f) m
  foldMap f  _          = mempty

instance (Traversable m) => Traversable (CutListT' m) where
  traverse f (CCons a m) = CCons <$> f a <*> traverse (traverse f) m
  traverse f CNil        = pure CNil
  traverse f CCut        = pure CCut

-- | A monad transformer that behaves like the list transformer,
-- but it allows Prolog's cut operator.
newtype CutListT m a = CutListT { unCutListT :: m (CutListT' m a) }
 deriving (Functor)

instance (Monad m) => Monad (CutListT m) where
  return a = CutListT $ return $ CCons a $ return CNil
  CutListT m >>= f = CutListT $ m >>= \x -> case x of
    CCons a m -> unCutListT $ f a +<>+ (CutListT m >>= f)
    CNil      -> return CNil
    CCut      -> return CCut

instance (Functor m, Monad m) => Applicative (CutListT m) where
  pure  = return
  (<*>) = ap

instance MonadTrans CutListT where
  lift m = CutListT $ liftM (\a -> CCons a $ return CNil) m

instance (Monad m) => MonadPlus (CutListT m) where
  mzero = CutListT $ return CNil 
  mplus = (+<>+)

instance (Foldable m) => Foldable (CutListT m) where
  foldMap f (CutListT m) = foldMap (foldMap f) m

instance (Traversable m) => Traversable (CutListT m) where
  traverse f (CutListT m) = CutListT <$> traverse (traverse f) m

instance (Monad m) => Monoid (CutListT m a) where
  mempty = mzero
  mappend = mplus

instance (Monad m) => MonoidRZero (CutListT m a) where
  rzero = CutListT $ return $ CCut

-- | Ignore the elements on the list and combine the monadic
-- computations.
retract :: (Monad m) => CutListT m a -> m ()
retract (CutListT m) = m >>= aux
 where
  aux (CCons _ m) = m >>= aux
  aux _           = return ()

-- | List with Prolog's cut operator.
type CutList = CutListT Identity

-- | Convert to a regular list.
toList :: CutList a -> [a]
toList (CutListT (Identity t)) = aux t
 where
  aux (CCons a (Identity t)) = a : aux t
  aux _                      = []

-- | Convert from a regular list
fromList :: [a] -> CutList a
fromList xs = CutListT $ Identity $ aux xs
 where
  aux (x : xs) = CCons x $ Identity $ aux xs
  aux _        = CNil

-- | Discard yet uninspected choices.
cut :: (Monad m) => CutListT m ()
cut = CutListT $ return CCut

-- | Discard the uninspected choices and fail the current branch of
-- computation. Equal to @'cut' '>>' 'mzero'@.
cutFail :: (Monad m) => CutListT m ()
cutFail = cut >> mzero

-- | Delimit the scope of cuts in the argument.
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

{- $takeWhile 

We implement the functon @'Data.List.takeWhile'@ using cuts:

@
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p xs = toList $ do
  x <- fromList xs
  when (not $ p x) cutFail
  return x
@
-}

