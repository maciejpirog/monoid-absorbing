{-# LANGUAGE DeriveFunctor #-}

module Data.Monoid.Zero
  (
    -- * Zero monoids
    MonoidZero(..),
    AdjoinZero(..),
    -- * Left zero monoids
    MonoidLZero(..),
    AdjoinLZero(..),
    -- * Right zero monoids
    MonoidRZero(..),
    AdjoinRZero(..),
  )
  where

import Data.Monoid (Monoid(..), Product(..), All(..), Any(..))

--
-- MONOID ZERO
--

-- | Class of zero monoids. Instances should satisfy:
--
-- * @'mappend' zero m = zero@
--
-- * @'mappend' m zero = zero@
class (Monoid m) => MonoidZero m where
  zero :: m

-- | A zero monoid freely generated by a monoid.
newtype AdjoinZero m = AdjoinZero { adjoinZero :: Maybe m }
 deriving (Functor, Eq, Ord)

instance (Show m) => Show (AdjoinZero m) where
  show (AdjoinZero (Just m)) = show m
  show (AdjoinZero Nothing)  = "*"

instance (Monoid m) => Monoid (AdjoinZero m) where
  mempty = AdjoinZero $ Just $ mempty
  AdjoinZero Nothing `mappend` _     = AdjoinZero Nothing
  _ `mappend` AdjoinZero Nothing     = AdjoinZero Nothing
  AdjoinZero (Just m) `mappend` AdjoinZero (Just n) =
    AdjoinZero $ Just (m `mappend` n)

instance (Monoid m) => MonoidZero (AdjoinZero m) where
  zero = AdjoinZero Nothing

instance (Num n) => MonoidZero (Product n) where
  zero = Product 0

instance MonoidZero All where
  zero = All False

instance MonoidZero Any where
  zero = Any True

instance MonoidZero () where
  zero = ()

instance (MonoidZero a, MonoidZero b) => MonoidZero (a, b) where
  zero = (zero, zero)

instance (MonoidZero a, MonoidZero b, MonoidZero c) => MonoidZero (a, b, c) where
  zero = (zero, zero, zero)

instance (MonoidZero a, MonoidZero b, MonoidZero c, MonoidZero d) => MonoidZero (a, b, c, d) where
  zero = (zero, zero, zero, zero)

instance (MonoidZero a, MonoidZero b, MonoidZero c, MonoidZero d, MonoidZero e) => MonoidZero (a, b, c, d, e) where
  zero = (zero, zero, zero, zero, zero)

--
-- MONOID LZERO
--

-- | Class of left zero monoids. Instances should satisfy:
--
-- * @'mappend' m lzero = lzero@
class (Monoid m) => MonoidLZero m where
  lzero :: m

-- | A left zero monoid freely generated by a monoid.
newtype AdjoinLZero m = AdjoinLZero { unAdjoinLZero :: (Bool, m) }
 deriving (Functor, Eq, Ord)

instance (Show m) => Show (AdjoinLZero m) where
  show (AdjoinLZero (b, m)) = (if b then "*" else "") ++ show m

instance (Monoid m) => Monoid (AdjoinLZero m) where
  mempty = AdjoinLZero (False, mempty)
  _ `mappend` AdjoinLZero (True, n) = AdjoinLZero (True, n)
  AdjoinLZero (b, m) `mappend` AdjoinLZero (False, n) =
      AdjoinLZero (b, m `mappend` n)

instance (Monoid m) => MonoidLZero (AdjoinLZero m) where
  lzero = AdjoinLZero (True, mempty)

instance (Num n) => MonoidLZero (Product n) where
  lzero = Product 0

instance MonoidLZero All where
  lzero = All False

instance MonoidLZero Any where
  lzero = Any True

instance MonoidLZero () where
  lzero = ()

instance (MonoidLZero a, MonoidLZero b) => MonoidLZero (a, b) where
  lzero = (lzero, lzero)

instance (MonoidLZero a, MonoidLZero b, MonoidLZero c) => MonoidLZero (a, b, c) where
  lzero = (lzero, lzero, lzero)

instance (MonoidLZero a, MonoidLZero b, MonoidLZero c, MonoidLZero d) => MonoidLZero (a, b, c, d) where
  lzero = (lzero, lzero, lzero, lzero)

instance (MonoidLZero a, MonoidLZero b, MonoidLZero c, MonoidLZero d, MonoidLZero e) => MonoidLZero (a, b, c, d, e) where
  lzero = (lzero, lzero, lzero, lzero, lzero)

--
-- MONOID RZERO
--

-- | Class of right zero monoids. Instances should satisfy:
--
-- * @'mappend' rzero m = rzero@
class (Monoid m) => MonoidRZero m where
  rzero :: m

-- | A right zero monoid freely generated by a monoid.
newtype AdjoinRZero m = AdjoinRZero { unAdjoinRZero :: (m, Bool) }
 deriving (Functor, Eq, Ord)

instance (Show m) => Show (AdjoinRZero m) where
  show (AdjoinRZero (m, b)) = show m ++ (if b then "*" else "")

instance (Monoid m) => Monoid (AdjoinRZero m) where
  mempty = AdjoinRZero (mempty, False)
  AdjoinRZero (m, True)  `mappend` _ = AdjoinRZero (m, True)
  AdjoinRZero (m, False) `mappend` AdjoinRZero (n, b) =
    AdjoinRZero (m `mappend` n, b)

instance (Monoid m) => MonoidRZero (AdjoinRZero m) where
  rzero = AdjoinRZero (mempty, True)

instance (Num n) => MonoidRZero (Product n) where
  rzero = Product 0

instance MonoidRZero All where
  rzero = All False

instance MonoidRZero Any where
  rzero = Any True

instance MonoidRZero () where
  rzero = ()

instance (MonoidRZero a, MonoidRZero b) => MonoidRZero (a, b) where
  rzero = (rzero, rzero)

instance (MonoidRZero a, MonoidRZero b, MonoidRZero c) => MonoidRZero (a, b, c) where
  rzero = (rzero, rzero, rzero)

instance (MonoidRZero a, MonoidRZero b, MonoidRZero c, MonoidRZero d) => MonoidRZero (a, b, c, d) where
  rzero = (rzero, rzero, rzero, rzero)

instance (MonoidRZero a, MonoidRZero b, MonoidRZero c, MonoidRZero d, MonoidRZero e) => MonoidRZero (a, b, c, d, e) where
  rzero = (rzero, rzero, rzero, rzero, rzero)
