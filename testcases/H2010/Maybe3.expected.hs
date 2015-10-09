{-# LANGUAGE Haskell2010 #-}

module Maybe3 where

instance Functor (Maybe' a) where
  fmap f m    = m >>= pure . f

instance Applicative (Maybe' a) where
  pure = Just'
  f1 <*> f2   = f1 >>= \v1 -> f2 >>= (pure . v1)

data Maybe' a b = Nothing' | Just' b

instance Monad (Maybe' a) where
    return = pure

    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x

    fail _ = Nothing'
