{-# LANGUAGE Haskell2010 #-}

module Maybe1 where

instance Functor Maybe' where
  fmap f m    = m >>= pure . f

instance Applicative Maybe' where
  pure = Just'
  f1 <*> f2   = f1 >>= \v1 -> f2 >>= (pure . v1)

data Maybe' a = Nothing' | Just' a

instance Monad Maybe' where
    return = pure

    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x

    fail _ = Nothing'
