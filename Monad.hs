module Monad where

import Prelude hiding (Maybe, Just, Nothing)

data Maybe a = Nothing | Just a

instance Monad Maybe where
    -- return :: a -> Maybe a
    return = Just 
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= f = Nothing   -- Exception propagation
    (Just a) >>= f = f a      -- "application"
{-
instance Monad List where
    return x = [x]
    xs >>= f = concatMap f xs  -- (>>=) = flip concatMap
-}

type State = Int

data STATE a = STATE (State -> (a,State))

instance Monad STATE where
    return a = STATE (\s -> (a, s))
    -- (>>=) :: (State -> (a,State)) -> (a -> (State -> (b,State))) -> (State -> (b,State))
    (STATE st) >>= f = STATE (\s -> let (a,s2)         = st s 
                                        STATE st'      = f a
                                     in st' s2)
    
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

t0 = Node (Leaf 'a') (Node (Leaf 'a')(Leaf 'a'))


label :: Tree a ->  Int -> (Tree (a,Int),Int)
label (Leaf a)   n = (Leaf (a, n),n+1)
label (Node x y) n = let (x',n' ) = label x n
                         (y',n'') = label y n'
                      in (Node x' y', n'')
                      
label' :: Tree a -> STATE (Tree (a,Int))
label' (Leaf a) = do 
                  n <- fresh
                  return (Leaf (a,n))
label' (Node x y) = do
                    x' <- label' x
                    y' <- label' y
                    return (Node x' y')
  

runState :: STATE a -> State -> a
runState (STATE st) = fst . st
  
fresh :: STATE Int
fresh = STATE (\n -> (n , n+1))

counter s = s
updateCounter s i = i

getCounter :: STATE Int
getCounter = STATE (\s -> (counter s, s))

putCounter :: Int -> STATE ()
putCounter i = STATE (\s -> (() ,updateCounter s i))
 
fresh' = 
 do
 c <- getCounter
 putCounter (c+1)
 return c
 

    