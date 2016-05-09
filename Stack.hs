module Stack(Stack,push,pop,top,emptyStack,stackEmpty) where

emptyStack:: Stack a
stackEmpty:: Stack a -> Bool
push :: a-> Stack a -> Stack a
pop :: Stack a -> Stack a
top :: Stack a -> a

{-  implementation with a constructor type -}
data Stack a = EmptyStk
             | Stk a (Stack a)

instance (Show a) => Show (Stack a) where
    showsPrec p  EmptyStk str = showChar '-' str
    showsPrec p (Stk x s) str = shows x (showChar '|' (shows s str))
             
emptyStack = EmptyStk

stackEmpty EmptyStk = True
stackEmpty _        = False

push x s = Stk x s

pop EmptyStk  = error "pop from an empty stack"
pop (Stk _ s) = s

top EmptyStk  = error "top from an empty stack"
top (Stk x _) =  x
{-   end of implementation with a constructor type-}

{-  list implementation  
newtype Stack a = Stk [a]

instance (Show a) => Show (Stack a) where
    showsPrec p (Stk [])     str = showChar '-' str
    showsPrec p (Stk (x:xs)) str
        = shows x (showChar '|' (shows (Stk xs) str))

emptyStack = Stk []

stackEmpty (Stk []) = True
stackEmpty (Stk _ ) = False

push x (Stk xs) = Stk (x:xs)

pop (Stk [])     = error "pop from an empty stack"
pop (Stk (_:xs)) = Stk  xs

top (Stk [])    = error "top from an empty stack"
top (Stk (x:_)) = x
   end of list implementation -}




