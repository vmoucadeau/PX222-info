
module Struct where

class Group a where
    zero :: a
    add :: a -> a -> a
    opp :: a -> a

class Group a => Ring a where
    one :: a
    mult :: a -> a -> a

class Ring a => Field a where
    inv :: a -> a
 
-- Exemple: le corps Z/7Z

instance Group Integer where
    zero :: Integer
    zero = 0
    add :: Integer -> Integer -> Integer
    add x y = x + y
    opp :: Integer -> Integer
    opp x = -x

instance Ring Integer where
    one :: Integer
    one = 1
    mult :: Integer -> Integer -> Integer
    mult x y = x * y

-- instance Field Float where (nan les float c'est nul !)
--     inv :: Float -> Float
--     inv x | x/=0 = 1/x

-- EOF
