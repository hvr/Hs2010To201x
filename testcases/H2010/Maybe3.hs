{-# LANGUAGE Haskell2010 #-}

module Maybe3 where

data Maybe' a b = Nothing' | Just' b

instance Monad (Maybe' a) where
    return = Just'

    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x

    fail _ = Nothing'
