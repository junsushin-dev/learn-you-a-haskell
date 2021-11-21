import Control.Monad.State

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((), newStack1) = push 3 stack
  (a , newStack2) = pop newStack1
  in pop newStack2

stackManip2 = do
  push 3
  a <- pop
  pop

-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Applicative (State s) => Monad (State s) where
--   return x = State $ \s -> (x,s)
--   (State h) >>= f = State $ \s -> let (a, newState) = h s
--                                       (State g) = f a
--                                   in  g newState

popState :: State Stack Int
popState = state $ \(x:xs) -> (x,xs)

pushState :: Int -> State Stack ()
pushState a = state $ \xs -> ((), a:xs)

stackManipState :: State Stack Int
stackManipState = do
  pushState 3
  popState
  popState

stackStuff :: State Stack ()
stackStuff = do
  a <- popState
  if a == 5
    then pushState 5
    else do
      pushState 3
      pushState 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManipState
  if a == 100
    then stackStuff
    else return ()

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get 
  if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]