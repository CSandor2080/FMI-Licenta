module MyPrelude where

import Prelude ()
 
id :: a -> a
id a = a
 
const :: a -> b -> a
const x _ = x
 
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x
 
(.) :: (b -> c) -> (a -> b) -> a -> c
(g . f) x = g (f x)