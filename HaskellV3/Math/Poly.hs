
module Math.Poly where

import Math.Struct

newtype Poly a = Px [a]

pxeq :: Group a => Poly a -> Poly a -> Bool
pxeq x y = let (Px lx,Px ly) = (wipepx x,wipepx y) in (==) lx ly

pxshow :: Group a => Poly a -> String
pxshow (Px x) = ' ' : (x >>= (\x -> show x ++ " "))

pxparse :: Group a => String -> Poly a
pxparse x = Px $ parse <$> words x

pxzer :: Group a => Poly a
pxzer = Px $ return zer

pxadd :: Group a => Poly a -> Poly a -> Poly a
pxadd (Px x) (Px y) = Px $ opList add zer x y

pxopp :: Group a => Poly a -> Poly a
pxopp (Px x) = Px $ map opp x

pxone :: Ring a => Poly a
pxone = Px $ return one

pxmul :: Ring a => Poly a -> Poly a -> Poly a
pxmul (Px []) _ = zer
pxmul (Px (x:xs)) (Px y) = wipepx $ add (Px $ map (mul x) y) ((\(Px z) -> Px (zer:z)) $ pxmul (Px xs) (Px y))

pxdie :: Field a => Poly a -> Poly a -> (Poly a,Poly a)
pxdie x y | (||) (degpx x < degpx y) (degpx y == (-1)) = (zer,wipepx x)
pxdie x y = let (tx,ty) = let (Px lx,Px ly) = (wipepx x,wipepx y) in (last lx,last ly) in
    let f = (.) (mul (Px [divs tx ty])) (apply (degpx x - degpx y) (\(Px z) -> Px (zer:z))) in
    let (q,r) = pxdie (subs x (f y)) y in (add (f one) q,r)

------------------------------------------------------------

instance Group a => Eq (Poly a) where
    (==) = pxeq

instance Group a => Show (Poly a) where
    show = pxshow

instance Group a => Parse (Poly a) where
    parse = pxparse

instance Group a => Group (Poly a) where
    zer = pxzer
    add = pxadd
    opp = pxopp

instance Field a => Ring (Poly a) where
    one = pxone
    mul = pxmul
    die = pxdie

------------------------------------------------------------

wipepx :: Group a => Poly a -> Poly a
wipepx (Px []) = Px []
wipepx (Px x) | last x == zer = wipepx (Px $ init x) | otherwise = Px x

degpx :: Group a => Poly a -> Int
degpx x = let (Px lx) = wipepx x in length lx - 1

fillpx :: Group a => Int -> Poly a -> Poly a
fillpx n (Px x) | (&&) (degpx (Px x) < n) (length x < n) = fillpx n (Px (x++[zer]))
fillpx n (Px x) | (&&) (degpx (Px x) < n) (length x > n) = fillpx n (Px (init x))
fillpx n x | degpx x < n = x

euclidpx :: Field a => (Poly a,Poly a,Poly a) -> (Poly a,Poly a,Poly a) -> (Poly a,Poly a,Poly a)
euclidpx x (_,_,y) | y == zer = x
euclidpx (ox,oy,or) (nx,ny,nr) = let (z,_) = die or nr in
    euclidpx (nx,ny,nr) (subs ox (mul z nx),subs oy (mul z ny),wipepx $ subs or (mul z nr))

------------------------------------------------------------

-- EOF
