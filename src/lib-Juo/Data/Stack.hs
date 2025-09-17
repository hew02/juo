module Data.Stack (
    Stack(..),
    empty,
    push,
    pop,
    size
) where

newtype Stack a = Stack [a] 
  deriving (Show,Eq)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

size :: Stack a -> Int
size (Stack items) = length items