{-# LANGUAGE Haskell2010 #-}

module Maybe2 where

data Maybe' a = Nothing' | Just' a

instance Functor Maybe' where
    fmap f (Just' v) = Just' (f v)
    fmap _ Nothing'  = Nothing'

instance Monad Maybe' where
    return = Just'

    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x
