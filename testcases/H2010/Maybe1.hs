{-# LANGUAGE Haskell2010 #-}

module Maybe1 where

data Maybe' a = Nothing' | Just' a

instance  Functor Maybe'  where
    fmap _ Nothing'       = Nothing'
    fmap f (Just' a)      = Just' (f a)

instance Applicative Maybe' where
    pure = Just'

    Just' f  <*> m       = fmap f m
    Nothing' <*> _m      = Nothing'

    Just' _m1 *> m2      = m2
    Nothing'  *> _m2     = Nothing'


instance Monad Maybe' where
    return = Just'

    Nothing' >>= _ = Nothing'
    Just' x  >>= f = f x

    fail _ = Nothing'

