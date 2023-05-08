
module Math.Bits(Zs2Z(Z2Z)) where

import Math.Struct

newtype Zs2Z = Z2Z Bool

z2zshow :: Zs2Z -> String
z2zshow (Z2Z False) = "0"
z2zshow (Z2Z True) = "1"

z2zparse :: Zs2Z -> String -> Zs2Z
z2zparse _ "0" = (Z2Z False)
z2zparse _ "1" = (Z2Z True)

z2zeq :: Zs2Z -> Zs2Z -> Bool
z2zeq (Z2Z x) (Z2Z y) = x == y

z2zzer :: Zs2Z
z2zzer = Z2Z False

z2zadd :: Zs2Z -> Zs2Z -> Zs2Z
z2zadd (Z2Z x) (Z2Z y) = Z2Z (x /= y)

z2zopp :: Zs2Z -> Zs2Z
z2zopp x = x

z2zone :: Zs2Z
z2zone = Z2Z True

z2zmul :: Zs2Z -> Zs2Z -> Zs2Z
z2zmul (Z2Z x) (Z2Z y) = Z2Z (x && y)

z2zdie :: Zs2Z -> Zs2Z -> (Zs2Z,Zs2Z)
z2zdie x (Z2Z True) = (Z2Z False,x)

z2zinv :: Zs2Z -> Zs2Z
z2zinv (Z2Z True) = Z2Z True

instance Show Zs2Z where
    show = z2zshow

instance Parse Zs2Z where
    parse = z2zparse

instance Eq Zs2Z where
    (==) = z2zeq

instance Group Zs2Z where
    zer = z2zzer
    add = z2zadd
    opp = z2zopp

instance Ring Zs2Z where
    one = z2zone
    mul = z2zmul
    die = z2zdie

instance Field Zs2Z where
    inv = z2zinv

-- EOF
