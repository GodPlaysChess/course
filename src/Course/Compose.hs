{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Apply
import Course.Applicative
import Course.Bind

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose  || a -> b, Compose f g a -> Compose f g b  g(A => X) f(X => Y)
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fab <$> (Compose x) = 
    Compose ((\fg -> (\g -> fab g) <$> fg) <$> x)
    --Compose ((f <$>) <$> g)

instance (Apply f, Apply g) =>
  Apply (Compose f g) where
-- Implement the (<*>) function for an Apply instance for Compose
  cfab <*> (Compose x) =
     error "todo: Course.Compose (<*>)#instance (Compose f g)"

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure a =
    Compose $ pure (pure a)

instance (Bind f, Bind g) =>
  Bind (Compose f g) where
-- Implement the (=<<) function for a Bind instance for Compose
  (=<<) =
    error "todo: Course.Compose (<<=)#instance (Compose f g)"
