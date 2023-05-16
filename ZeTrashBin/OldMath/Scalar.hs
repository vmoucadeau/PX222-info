
module OldMath.Scalar(Zs2Z) where

import OldMath.Struct

------------------------------------------------------------
-- ------- Définition du corps des scalaires Z/2Z ------- --
------------------------------------------------------------

newtype Zs2Z = Z2Z Int

z2zshow :: Zs2Z -> String
z2zshow (Z2Z x) = show (mod x 2)

z2zeq :: Zs2Z -> Zs2Z -> Bool
z2zeq (Z2Z x) (Z2Z y) = (mod x 2) == (mod y 2)

z2zzer :: Zs2Z
z2zzer = Z2Z 0

z2zadd :: Zs2Z -> Zs2Z -> Zs2Z
z2zadd (Z2Z x) (Z2Z y) = Z2Z (mod (x+y) 2)

z2zopp :: Zs2Z -> Zs2Z
z2zopp (Z2Z 0) = Z2Z 0
z2zopp (Z2Z 1) = Z2Z (2-1)

z2zone :: Zs2Z
z2zone = Z2Z 1

z2zmul :: Zs2Z -> Zs2Z -> Zs2Z
z2zmul (Z2Z x) (Z2Z y) = Z2Z (mod (x*y) 2)

z2zinv :: Zs2Z -> Zs2Z
z2zinv (Z2Z 1) = Z2Z 1

instance Show Zs2Z where
    show = z2zshow

instance Eq Zs2Z where
    (==) = z2zeq

instance Group Zs2Z where
    zer = z2zzer
    add = z2zadd
    opp = z2zopp

instance Ring Zs2Z where
    one = z2zone
    mul = z2zmul

instance Field Zs2Z where
    inv = z2zinv

-- EOF
