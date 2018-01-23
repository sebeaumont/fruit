module Math.Category.Hask.State where

-- model the state monad in peace
--import qualified Prelude


-- random

--random :: Double -> (Double, Integer)
--random g = (g*2.12399E13, floor g)

newtype State s a = State { runState :: s -> (a, s) }

--instance Functor (State s) where
-- to do instances elsewhere
--  fmap = 

  
