
module Scalaire where

import Struct

-----------------------------------------------------
-- -------- Fonctions génériques de calcul ------- --
-----------------------------------------------------

opList :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y:opList f n xs ys

-----------------------------------------------------
-- --- Définition du corps des scalaires, Z/2Z --- --
-----------------------------------------------------

newtype Zs2Z = Z2Z Integer deriving (Show,Eq)

z2zzer :: Zs2Z
z2zzer = Z2Z 0

z2zadd :: Zs2Z -> Zs2Z -> Zs2Z
z2zadd (Z2Z x) (Z2Z y) = Z2Z (mod (x+y) 2)

z2zopp :: Zs2Z -> Zs2Z
z2zopp (Z2Z x) = Z2Z (-x)

z2zone :: Zs2Z
z2zone = Z2Z 1

z2zmul :: Zs2Z -> Zs2Z -> Zs2Z
z2zmul (Z2Z x) (Z2Z y) = Z2Z (mod (x*y) 2)

z2zinv :: Zs2Z -> Zs2Z
z2zinv (Z2Z 1) = Z2Z 1

instance Group Zs2Z where
    zer :: Zs2Z
    zer = z2zzer
    add :: Zs2Z -> Zs2Z -> Zs2Z
    add = z2zadd
    opp :: Zs2Z -> Zs2Z
    opp =z2zopp

instance Ring Zs2Z where
    one :: Zs2Z
    one = z2zone
    mul :: Zs2Z -> Zs2Z -> Zs2Z
    mul = z2zmul

instance Field Zs2Z where
    inv :: Zs2Z -> Zs2Z
    inv = z2zinv

-- EOF
