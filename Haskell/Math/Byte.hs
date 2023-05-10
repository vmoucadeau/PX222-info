
module Math.Byte(GF256(F8)) where

import Math.Struct
import Math.Poly
import Math.Bits

newtype GF256 = F8 (Poly Zs2Z)

f8show :: GF256 -> String
f8show (F8 (Px (x0:x1:x2:x3:x4:x5:x6:x7:xs))) | degpol (Px (x0:x1:x2:x3:x4:x5:x6:x7:xs)) > 7
    = (++) ("{" ++ f8show (F8 $ Px xs) ++ "}") (f8show (F8 $ Px (x0:x1:x2:x3:x4:x5:x6:x7:[])))
f8show (F8 (Px (x0:x1:x2:x3:x4:x5:x6:x7:[])))
    = "[" ++ (hexaprint ((x7:x6:x5:x4:[]) >>= show)) ++ (hexaprint ((x3:x2:x1:x0:[]) >>= show)) ++ "]"
f8show (F8 x) = f8show (F8 $ fillpol 8 x)

f8parse :: GF256 -> String -> GF256
f8parse _ [x1,x0] = F8 $ Px $ map (\x -> parse (Z2Z True) [x]) $ reverse (hexaparse [x1] ++ hexaparse [x0])

f8eq :: GF256 -> GF256 -> Bool
f8eq (F8 x) (F8 y) = x == y

f8zer :: GF256
f8zer = F8 $ fillpol 8 zer

f8add :: GF256 -> GF256 -> GF256
f8add (F8 x) (F8 y) = F8 $ fillpol 8 (add x y)

f8opp :: GF256 -> GF256
f8opp (F8 x) = F8 $ fillpol 8 (opp x)

f8one :: GF256
f8one = F8 $ fillpol 8 one

f8mul :: GF256 -> GF256 -> GF256
f8mul (F8 x) (F8 y) = let (_,F8 z) = die (F8 (mul x y)) mx in F8 $ fillpol 8 z

f8die :: GF256 -> GF256 -> (GF256,GF256)
f8die (F8 x) (F8 y) = let (q,r) = die x y in (F8 q, F8 r)

f8inv :: GF256 -> GF256
f8inv (F8 x) | (degpol x == -1) = zer
f8inv (F8 x) | (&&) (degpol x - 1 < 7) (degpol x + 1 > 0) = let F8 r = mx in
    let (_,z,_) = euclidpol (one,zer,r) (zer,one,x) in F8 $ wipepol z

instance Show GF256 where
    show = f8show

instance Parse GF256 where
    parse = f8parse

instance Eq GF256 where
    (==) = f8eq

instance Group GF256 where
    zer = f8zer
    add = f8add
    opp = f8opp

instance Ring GF256 where
    one = f8one
    mul = f8mul
    die = f8die

instance Field GF256 where
    inv = f8inv

------------------------------------------------------------
-- ------------ Example polynomials in GF256 ------------ --
------------------------------------------------------------

ex0 :: GF256
ex0 = F8 $ Px []

ex1 :: GF256
ex1 = F8 $ Px [zer,one,zer,one]

ex2 :: GF256
ex2 = F8 $ Px [one,zer,zer,one,zer]

ex3 :: GF256
ex3 = F8 $ Px [one,one,one,zer,one,zer,one,zer]

ex4 :: GF256
ex4 = F8 $ Px [one,one,zer,zer,one,zer,zer]

ex5 :: GF256
ex5 = F8 $ Px [zer,one,one,one,one,one,one,one]

ex6 :: GF256
ex6 = F8 $ Px [one,one,zer,zer,zer,zer,zer,one,zer,zer]

mx :: GF256
mx = F8 $ Px [one,one,zer,one,one,zer,zer,zer,one]

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

-- EOF
