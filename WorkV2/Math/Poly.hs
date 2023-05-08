
module Math.Poly where

import Math.Struct

newtype Poly a = Px [a]

polshow :: Show a => Poly a -> String
polshow (Px x) = (reverse x) >>= show

polparse :: Parse a => Poly a -> String -> Poly a
polparse (Px (t:_)) x = Px $ map (parse t) $ reverse $ words x

poleq :: Group a => Poly a -> Poly a -> Bool
poleq x y = let (Px lx,Px ly) = (wipepol x,wipepol y) in lx == ly

polzer :: Group a => Poly a
polzer = Px $ return zer

poladd :: Group a => Poly a -> Poly a -> Poly a
poladd (Px x) (Px y) = Px $ opList add zer x y

polopp :: Group a => Poly a -> Poly a
polopp (Px x) = Px $ map opp x

polone :: Ring a => Poly a
polone = Px $ return one

polmul :: Ring a => Poly a -> Poly a -> Poly a
polmul (Px []) _ = zer
polmul (Px (x:xs)) (Px y) = wipepol $ add (Px (map (mul x) y)) ((\(Px z)->Px(zer:z)) $ polmul (Px xs) (Px y))

poldie :: Field a => Poly a -> Poly a -> (Poly a,Poly a)
poldie x y | (||) (degpol x < degpol y) (degpol y == (-1)) = (zer,wipepol x)
poldie x y = let (tx,ty) = let (Px lx,Px ly) = (wipepol x,wipepol y) in (last lx,last ly) in
    let f = (.) (mul (Px [divs tx ty])) (apply (degpol x - degpol y) (\(Px z)->Px(zer:z))) in
    let (q,r) = poldie (subs x (f y)) y in (add (f one) q,r)

instance Show a => Show (Poly a) where
    show = polshow

instance Parse a => Parse (Poly a) where
    parse = polparse

instance Group a => Eq (Poly a) where
    (==) = poleq

instance Group a => Group (Poly a) where
    zer = polzer
    add = poladd
    opp = polopp

instance Field a => Ring (Poly a) where
    one = polone
    mul = polmul
    die = poldie

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

apply :: Int -> (a -> a) -> a -> a
apply 0 _ x = x
apply n f x = apply (n-1) f (f x)

opList :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y:opList f n xs ys

wipepol :: Group a => Poly a -> Poly a
wipepol (Px []) = Px []
wipepol (Px x) | last x == zer = wipepol (Px $ init x) | otherwise = Px x

fillpol :: Group a => Int -> Poly a -> Poly a
fillpol n (Px x) | (&&) (degpol (Px x) < n) (length x < n) = fillpol n (Px (x++[zer]))
fillpol n (Px x) | (&&) (degpol (Px x) < n) (length x > n) = fillpol n (Px (init x))
fillpol n x | degpol x < n = x

degpol :: Group a => Poly a -> Int
degpol x = let (Px lx) = wipepol x in length lx - 1

euclidpol :: Field a => (Poly a,Poly a,Poly a) -> (Poly a,Poly a,Poly a) -> (Poly a,Poly a,Poly a)
euclidpol x (_,_,y) | y == zer = x
euclidpol (x0,x1,or) (y0,y1,nr) = let (z,_) = poldie or nr in
    euclidpol (y0,y1,nr) (subs x0 (mul z y0),subs x1 (mul z y1),wipepol $ subs or (mul z nr))

hexaprint :: String -> String
hexaprint "0000" = "0"
hexaprint "0001" = "1"
hexaprint "0011" = "3"
hexaprint "0010" = "2"
hexaprint "0110" = "6"
hexaprint "0111" = "7"
hexaprint "0101" = "5"
hexaprint "0100" = "4"
hexaprint "1100" = "C"
hexaprint "1101" = "D"
hexaprint "1111" = "F"
hexaprint "1110" = "E"
hexaprint "1010" = "A"
hexaprint "1011" = "B"
hexaprint "1001" = "9"
hexaprint "1000" = "8"

hexaparse :: String -> String
hexaparse "0" = "0000"
hexaparse "1" = "0001"
hexaparse "3" = "0011"
hexaparse "2" = "0010"
hexaparse "6" = "0110"
hexaparse "7" = "0111"
hexaparse "5" = "0101"
hexaparse "4" = "0100"
hexaparse "C" = "1100"
hexaparse "D" = "1101"
hexaparse "F" = "1111"
hexaparse "E" = "1110"
hexaparse "A" = "1010"
hexaparse "B" = "1011"
hexaparse "9" = "1001"
hexaparse "8" = "1000"

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

-- EOF
