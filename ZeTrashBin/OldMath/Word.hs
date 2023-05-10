
module Math.Word(GF4X) where

import Math.Struct
import Math.Byte

------------------------------------------------------------
-- ---------- Définition de l'anneau fini GF4X ---------- --
------------------------------------------------------------

newtype GF4X = W4 [GF256]

w4show :: GF4X -> String
w4show (W4 x) = (reverse x) >>= show

w4eq :: GF4X -> GF4X -> Bool
w4eq (W4 x) (W4 y) = foldl (&&) True (opList (==) zer x y)

w4zer :: GF4X
w4zer = W4 [zer]

w4add :: GF4X -> GF4X -> GF4X
w4add (W4 x) (W4 y) = W4 $ opList add zer x y

w4opp :: GF4X -> GF4X
w4opp (W4 x) = W4 $ map opp x

w4one :: GF4X
w4one = W4 [one]

w4mul :: GF4X -> GF4X -> GF4X
w4mul (W4 [x0,x1,x2,x3]) (W4 [y0,y1,y2,y3]) = let
    z0 = add (add (mul x0 y0) (mul x1 y3)) (add (mul x2 y2) (mul x3 y1))
    z1 = add (add (mul x0 y1) (mul x1 y0)) (add (mul x2 y3) (mul x3 y2))
    z2 = add (add (mul x0 y2) (mul x1 y1)) (add (mul x2 y0) (mul x3 y3))
    z3 = add (add (mul x0 y3) (mul x1 y2)) (add (mul x2 y1) (mul x3 y0))
    in W4 [z0,z1,z2,z3]
w4mul x y = w4mul (bytew4 x) (bytew4 y)

instance Show GF4X where
    show = w4show

instance Eq GF4X where
    (==) = w4eq

instance Group GF4X where
    zer = w4zer
    add = w4add
    opp = w4opp

instance Ring GF4X where
    one = w4one
    mul = w4mul

------------------------------------------------------------
-- ----------- Polynômes d'exemple dans GF4X ------------ --
------------------------------------------------------------

ex0 :: GF4X
ex0 = W4 []

ex1 :: GF4X
ex1 = W4 [zer,one,one,zer]

ax :: GF4X
ax = W4 [one,zer,zer,zer,one]

------------------------------------------------------------
-- ---------- Fonctions auxiliaires pour GF4X ----------- --
------------------------------------------------------------

opList :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y:opList f n xs ys

wipew4 :: GF4X -> GF4X
wipew4 (W4 []) = W4 []
wipew4 (W4 x) | last x == zer = wipew4 (W4 (init x)) | otherwise = W4 x

degw4 :: GF4X -> Int
degw4 x = let (W4 y) = wipew4 x in length y - 1

bytew4 :: GF4X -> GF4X
bytew4 (W4 x) | (&&) (degw4 (W4 x) < 4) (length x < 4) = bytew4 (W4 (x++[zer]))
bytew4 (W4 x) | (&&) (degw4 (W4 x) < 4) (length x > 4) = bytew4 (W4 (init x))
bytew4 x | degw4 x < 4 = x

-- EOF
