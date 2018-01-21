{-# LANGUAGE ConstrainedClassMethods #-}

module Math.Category.Hask.Functor where

import qualified Prelude

class Functor f where
  fmap :: Functor f => (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f c
  (<*) :: f a -> f b -> f a
  
class Applicative m => Monad m where
  (>==) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a

type Unit = ()

data Bool = True | False 
data List a = Unit | Cons a (List a)

instance Functor [] where
  fmap = Prelude.map

