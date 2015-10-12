Automatic H2010 to H201x Refactoring
====================================

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/alanz/Hs2010To201X.png?branch=master
[travis]: https://travis-ci.org/alanz/Hs2010To201X
[badge-hackage]: https://img.shields.io/hackage/v/Hs2010To201X.svg?dummy
[hackage]: https://hackage.haskell.org/package/Hs2010To201X
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/alanz/Hs2010To201X/blob/master/LICENSE

Typeclass Refactoring
---------------------

### `Monad`-hierarchy

#### Haskell Report Definitions

Haskell 2010 defines only the `Functor` and `Monad` typeclasses:

```haskell
class  Functor f  where
    fmap    :: (a -> b) -> f a -> f b

class  Monad m  where
    return  :: a -> m a

    (>>=)   :: m a -> (a -> m b) -> m b

    (>>)    :: m a -> m b -> m b
    m >> k  =  m >>= \_ -> k
    
    fail    :: String -> m a
    fail s  = error s
```

With Haskell 201x this evolves into

```haskell
class  Functor f  where
    fmap    :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    (<*>) :: f (a -> b) -> f a -> f b

    -- | Sequence actions, discarding the value of the first argument.
    (*>) :: f a -> f b -> f b
    (*>) = …

    -- | Sequence actions, discarding the value of the second argument.
    (<*) :: f a -> f b -> f a
    (<*) = …

class Applicative m => Monad m where
    -- | Monadic bind
    (>>=) :: forall a b. m a -> (a -> m b) -> m b

class Monad m => MonadFail m where
    -- | Invoked on pattern-failures in monadic binds
    fail :: String -> m a

-- Backward-compatible top-level alias bindings

(>>) :: Functor m => m a -> m b -> m b
(>>) = (*>)

return :: Functor m => a -> m a
return = pure
```


#### GHC: Interim Haskell 2010 + `Applicative` definitions

There are multiple intermediate stages:

 - GHC 7.0 to 7.8: Haskell 2010 `Functor`/`Monad` class hierarchy + non-superclass `Applicative`
 - GHC 7.10:       Haskell 201x `Functor`/`Monad`/`Applicative` class hierarchy (sans `MonadFail`) with legacy `return`/`(>>)`/`fail`
 - GHC 8.0 to 8.X: Haskell 201x `Functor`/`Monad`/`Applicative`/`MonadFail` class hierarchy with legacy `return`/`(>>)`/`fail`
 - GHC 8.X+2:      Haskell 201x

#### Rewrite rules

Converting from plain Haskell2010 to Haskell201x is simple:

- Create `Functor` instance if missing with `fmap` defined in terms of `(>>=)` and `pure`
- Create `Applicative` instance and define `(<*>)` in terms of `(>>=)` and `pure`
- Rewrite `Monad(return)` implementation into `Applicative(pure)` implementation
- If exists, rewrite `Monad((>>))` implementation into `Applicative((*>))` implementation
- If `fail` is defined, create `MonadFail` instance with that `fail` method implementation

TODO: Give rules based on AMP-migration-guide definitions
