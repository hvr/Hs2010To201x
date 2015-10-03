-- {-# LANGUAGE Haskell201x #-}

module Maybe1 where

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap f m    = m >>= pure . f

instance Applicative Maybe' where
    pure = Just'
    f1 <*> f2   = f1 >>= \v1 -> f2 >>= (pure . v1)

instance Monad Maybe' where
    Nothing' >>= _  = Nothing'
    Just' x  >>= f  = f x

instance MonadFail Maybe' where
    fail _ = Nothing'
