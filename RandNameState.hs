module RandNameState(RandNameState,
                     freshName, freshNameState,
                     evalRandState) where

import Control.Monad.State.Lazy
import Control.Monad.Random

type RandNameState a = RandT StdGen (State NameSource) a

freshName :: RandNameState String
freshName = do
  n <- lift freshNameState
  return n

freshNameState :: State NameSource String
freshNameState = do
  ns <- get
  let (newNS, n) = nextName ns in
   do
     put newNS
     return n

evalRandState :: Int -> RandNameState a -> a
evalRandState seed comp =
  evalState (evalRandT comp (mkStdGen seed)) (nameSource "i" 0)

data NameSource = NameSource String Int
                  deriving (Eq, Ord, Show)

nameSource p n = NameSource p n

nextName (NameSource p n) = (NameSource p (n+1), p ++ show n)
