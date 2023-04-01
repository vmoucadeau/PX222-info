
module Struct where

class Group a where
    zer :: a
    add :: a -> a -> a
    opp :: a -> a
    subs :: a -> a -> a
    subs x y = add x (opp y)

class Group a => Ring a where
    one :: a
    mul :: a -> a -> a

class Ring a => Field a where
    inv :: a -> a
    divs :: a -> a -> a
    divs x y = mul x (inv y)

----------------------------------------------------
-- ----------- Exemple: le corps Z/7Z ----------- --
----------------------------------------------------

newtype Zs7Z = Z7Z Integer deriving (Show,Eq)

z7zzer :: Zs7Z
z7zzer = Z7Z 0

z7zadd :: Zs7Z -> Zs7Z -> Zs7Z
z7zadd (Z7Z x) (Z7Z y) = Z7Z (mod (x+y) 7)

z7zopp :: Zs7Z -> Zs7Z
z7zopp (Z7Z 0) = Z7Z 0
z7zopp (Z7Z x) = Z7Z (7-x)

z7zone :: Zs7Z
z7zone = Z7Z 1

z7zmul :: Zs7Z -> Zs7Z -> Zs7Z
z7zmul (Z7Z x) (Z7Z y) = Z7Z (mod (x*y) 7)

z7zinv :: Zs7Z -> Zs7Z
z7zinv (Z7Z 1) = Z7Z 1
z7zinv (Z7Z 2) = Z7Z 4
z7zinv (Z7Z 3) = Z7Z 5
z7zinv (Z7Z 4) = Z7Z 2
z7zinv (Z7Z 5) = Z7Z 3
z7zinv (Z7Z 6) = Z7Z 6

instance Group Zs7Z where
    zer = z7zzer
    add = z7zadd
    opp = z7zopp

instance Ring Zs7Z where
    one = z7zone
    mul = z7zmul

instance Field Zs7Z where
    inv = z7zinv

-- EOF
