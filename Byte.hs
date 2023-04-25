
module GF where

import Struct
import Scalaire

newtype GF256 = F8 [Zs2Z]

------------------------------------------------------------
-- ----------- Polynômes d'exemple dans GF256 ----------- --
------------------------------------------------------------

ex0 :: GF256
ex0 = F8 []

ex1 :: GF256
ex1 = F8 [zer,one,zer,one]

ex2 :: GF256
ex2 = F8 [one,zer,zer,one,zer]

ex3 :: GF256
ex3 = F8 [one,one,one,zer,one,zer,one,zer]

ex4 :: GF256
ex4 = F8 [one,one,zer,zer,one,zer,zer]

ex5 :: GF256
ex5 = F8 [zer,one,one,one,one,one,one,one]

ex6 :: GF256
ex6 = F8 [one,one,zer,zer,zer,zer,zer,one,zer,zer]

mx :: GF256
mx = F8 [one,one,zer,one,one,zer,zer,zer,one]

------------------------------------------------------------
-- ---------- Fonctions auxiliaires pour GF256 ---------- --
------------------------------------------------------------

apply :: Int -> (a -> a) -> a -> a
apply 0 _ x = x
apply n f x = apply (n-1) f (f x)

opList :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y:opList f n xs ys

wipef8 :: GF256 -> GF256
wipef8 (F8 []) = F8 []
wipef8 (F8 x) | last x == zer = wipef8 (F8 (init x)) | otherwise = F8 x

multbyx :: GF256 -> GF256
multbyx (F8 (x0:x1:x2:x3:x4:x5:x6:(Z2Z 0):[])) = F8 (zer:x0:x1:x2:x3:x4:x5:x6:[])
multbyx (F8 (x0:x1:x2:x3:x4:x5:x6:(Z2Z 1):[])) = wipef8 $ subs (F8 (zer:x0:x1:x2:x3:x4:x5:x6:[one])) mx
multbyx (F8 x) | length x < 8 = F8 (zer:x)

degf8 :: GF256 -> Int
degf8 x = let (F8 y) = wipef8 x in length y - 1

divEf8 :: GF256 -> GF256 -> (GF256,GF256)
divEf8 x y | degf8 x < degf8 y = (zer,wipef8 x)
divEf8 x y = let f = apply (degf8 x - degf8 y) (\(F8 x)->F8(zer:x)) in
    let (q,r) = (divEf8 (subs x (f y)) y) in (add (f one) q,r)

polymul :: GF256 -> GF256 -> GF256
polymul (F8 []) _ = zer
polymul (F8 (x:xs)) (F8 y) = add (F8 (map (mul x) y)) ((\(F8 x)->F8(zer:x)) $ polymul (F8 xs) (F8 y))

euclidf8 :: (GF256,GF256,GF256) -> (GF256,GF256,GF256) -> (GF256,GF256,GF256)
euclidf8 x (_,_,y) | y == zer = x
euclidf8 (x0,x1,or) (y0,y1,nr) = let (z,_) = divEf8 or nr in
    euclidf8 (y0,y1,nr) (subs x0 (mul z y0),subs x1 (mul z y1),wipef8 $ subs or (polymul z nr))

bytef8 :: GF256 -> GF256
bytef8 (F8 x) | (&&) (degf8 (F8 x) < 8) (length x < 8) = bytef8 (F8 (x++[zer]))
bytef8 (F8 x) | (&&) (degf8 (F8 x) < 8) (length x > 8) = bytef8 (F8 (init x))
bytef8 x | degf8 x < 8 = x

------------------------------------------------------------
-- ----------- Fonctions pratiques pour GF256 ----------- --
------------------------------------------------------------

f8show1 :: GF256 -> String
f8show1 (F8 []) = " But it's empty... "
f8show1 (F8 x) = foldl (++) [] (map show (reverse x))

f8show2 :: GF256 -> String
f8show2 (F8 x) | degf8 (F8 x) > 7 = (++) (show $ last x) (f8show2 (F8 (init x)))
f8show2 (F8 x) | length x == 8 = (++) " > " (foldl (++) [] (map show (reverse x)))
f8show2 x = f8show2 $ bytef8 x

hexaprint :: [Zs2Z] -> String
hexaprint [Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 0] = "0"
hexaprint [Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 1] = "1"
hexaprint [Z2Z 0,Z2Z 0,Z2Z 1,Z2Z 1] = "3"
hexaprint [Z2Z 0,Z2Z 0,Z2Z 1,Z2Z 0] = "2"
hexaprint [Z2Z 0,Z2Z 1,Z2Z 1,Z2Z 0] = "6"
hexaprint [Z2Z 0,Z2Z 1,Z2Z 1,Z2Z 1] = "7"
hexaprint [Z2Z 0,Z2Z 1,Z2Z 0,Z2Z 1] = "5"
hexaprint [Z2Z 0,Z2Z 1,Z2Z 0,Z2Z 0] = "4"
hexaprint [Z2Z 1,Z2Z 1,Z2Z 0,Z2Z 0] = "C"
hexaprint [Z2Z 1,Z2Z 1,Z2Z 0,Z2Z 1] = "D"
hexaprint [Z2Z 1,Z2Z 1,Z2Z 1,Z2Z 1] = "F"
hexaprint [Z2Z 1,Z2Z 1,Z2Z 1,Z2Z 0] = "E"
hexaprint [Z2Z 1,Z2Z 0,Z2Z 1,Z2Z 0] = "A"
hexaprint [Z2Z 1,Z2Z 0,Z2Z 1,Z2Z 1] = "B"
hexaprint [Z2Z 1,Z2Z 0,Z2Z 0,Z2Z 1] = "9"
hexaprint [Z2Z 1,Z2Z 0,Z2Z 0,Z2Z 0] = "8"

f8show3 :: GF256 -> String
f8show3 (F8 (x0:x1:x2:x3:x4:x5:x6:x7:xs)) | degf8 (F8 (x0:x1:x2:x3:x4:x5:x6:x7:xs)) > 7
    = (++) ("{" ++ f8show1 (F8 xs) ++ "}") (f8show3 (F8 (x0:x1:x2:x3:x4:x5:x6:x7:[])))
f8show3 (F8 (x0:x1:x2:x3:x4:x5:x6:x7:[]))
    = "[" ++ (hexaprint (x7:x6:x5:x4:[])) ++ (hexaprint (x3:x2:x1:x0:[])) ++ "]"
f8show3 x = f8show3 $ bytef8 x

------------------------------------------------------------
-- ----------- Définition du corps fini GF256 ----------- --
------------------------------------------------------------

f8show :: GF256 -> String
f8show = f8show3

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
f8mul (F8 (x:xs)) (F8 y) = wipef8 $ add (F8 (map (mul x) y)) (multbyx $ f8mul (F8 xs) (F8 y))

f8inv :: GF256 -> GF256
f8inv x | (&&) (degf8 x - 1 < 7) (degf8 x + 1 > 0) = let (_,z,_) = euclidf8 (one,zer,mx) (zer,one,x) in wipef8 z

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
