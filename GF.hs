
module GF where

import Struct
import Scalaire

----------------------------------------------------------
-- ---------- Polynômes d'exemple pour GF256 ---------- --
----------------------------------------------------------

empty :: a -> a
empty x = x

ex0 :: GF256
ex0 = F8 []

ex1 :: GF256
ex1 = F8 [zer,one,zer,one]

ex2 :: GF256
ex2 = F8 [one,zer,zer,one,zer]

ex3 :: GF256
ex3 = F8 [one,one,one,zer,one,zer,one,zer]

ex4 :: GF256
ex4 = F8 [one,one,zer,zer,one,zer,zer,zer]

ex5 :: GF256
ex5 = F8 [zer,one,one,one,one,one,one,one]

mx :: GF256
mx = F8 [one,one,zer,one,one,zer,zer,zer,one]

----------------------------------------------------------
-- ---------- Fonctions secondaires de GF256 ---------- --
----------------------------------------------------------

clearf8 :: GF256 -> GF256
clearf8 (F8 (x0:x1:x2:x3:x4:x5:x6:x7:xs)) = F8 (x0:x1:x2:x3:x4:x5:x6:x7:[])
clearf8 x = x

multbyx :: GF256 -> GF256
multbyx (F8 (x0:x1:x2:x3:x4:x5:x6:(Z2Z 0):[])) = F8 (zer:x0:x1:x2:x3:x4:x5:x6:[])
multbyx (F8 (x0:x1:x2:x3:x4:x5:x6:(Z2Z 1):[])) = clearf8 $ subs (F8 (zer:x0:x1:x2:x3:x4:x5:x6:[one])) mx
multbyx (F8 x) | length x < 8 = F8 (zer:x)

----------------------------------------------------------
-- ---------- Définition du corps fini GF256 ---------- --
----------------------------------------------------------

newtype GF256 = F8 [Zs2Z]

f8show :: GF256 -> String
f8show (F8 x) = foldl (++) [] (map show (reverse x))

f8eq :: GF256 -> GF256 -> Bool
f8eq (F8 x) (F8 y) = foldl (&&) True (opList (==) zer x y)

f8zer :: GF256
f8zer = F8 [zer]

f8add :: GF256 -> GF256 -> GF256
f8add (F8 x) (F8 y) = F8 $ opList add zer x y

f8opp :: GF256 -> GF256
f8opp (F8 x) = F8 $ map opp x

f8one :: GF256
f8one = F8 [one]

f8mul :: GF256 -> GF256 -> GF256
f8mul (F8 []) _ = zer
f8mul (F8 (x:xs)) (F8 y) = f8add (F8 (map (mul x) y)) (multbyx $ f8mul (F8 xs) (F8 y))

f8inv :: GF256 -> GF256
f8inv (F8 x) = zer -- I'll do it next week, don't you dare steal it ^^

instance Show GF256 where
    show = f8show

instance Eq GF256 where
    (==) = f8eq

instance Group GF256 where
    zer = f8zer
    add = f8add
    opp = f8opp

instance Ring GF256 where
    one = f8one
    mul = f8mul

instance Field GF256 where
    inv = f8inv

-- EOF
