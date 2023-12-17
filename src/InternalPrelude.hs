module InternalPrelude where

import Prelude

bool :: a -> a -> Bool -> a
bool a b t = if t then a else b

boolEither :: a -> b -> Bool -> Either a b
boolEither l r t = bool (Left l) (Right r) t

infixr 9 ^
(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)
