{-# LANGUAGE Haskell2010 #-}

module Maybe1 where

data Maybe' a = Nothing' | Just' a

instance Monad Maybe' where
    return = Just'

    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x

    fail _ = Nothing'
