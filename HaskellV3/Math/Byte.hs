
module Math.Byte(GF256(F8)) where

import Math.Struct
import Math.Poly
import Math.Bits

newtype GF256 = F8 (Poly Zs2Z)

f8eq :: GF256 -> GF256 -> Bool
f8eq (F8 x) (F8 y) = (==) x y

f8show :: GF256 -> String
f8show (F8 (Px (x0:x1:x2:x3:x4:x5:x6:x7:xs))) | degpx (Px (x0:x1:x2:x3:x4:x5:x6:x7:xs)) > 7
    = (++) ("{" ++ f8show (F8 $ Px xs) ++ "}") (f8show (F8 $ Px (x0:x1:x2:x3:x4:x5:x6:x7:[])))
f8show (F8 (Px (x0:x1:x2:x3:x4:x5:x6:x7:[]))) = (hexaprint ([x7,x6,x5,x4] >>= show)) ++ (hexaprint ([x3,x2,x1,x0] >>= show))
f8show (F8 x) = f8show (F8 $ fillpx 8 x)

f8parse :: String -> GF256
f8parse [x1,x0] = F8 $ Px $ parse <$> return <$> reverse (hexaparse [x1] ++ hexaparse [x0])
f8parse x = f8parse $ filter ((/=) ' ') x

f8zer :: GF256
f8zer = F8 $ fillpx 8 zer

f8add :: GF256 -> GF256 -> GF256
f8add (F8 x) (F8 y) = F8 $ fillpx 8 $ add x y

f8opp :: GF256 -> GF256
f8opp (F8 x) = F8 $ fillpx 8 x

f8one :: GF256
f8one = F8 $ fillpx 8 one

f8mul :: GF256 -> GF256 -> GF256
f8mul (F8 x) (F8 y) = let (_,F8 z) = die (F8 $ mul x y) mx in F8 $ fillpx 8 z

f8die :: GF256 -> GF256 -> (GF256,GF256)
f8die (F8 x) (F8 y) = let (q,r) = die x y in (F8 q, F8 r)

f8inv :: GF256 -> GF256
f8inv (F8 x) | (degpx x == -1) = zer
f8inv (F8 x) | (&&) (degpx x - 1 < 7) (degpx x + 1 > 0) = let (F8 r) = mx in
    let (_,z,_) = euclidpx (one,zer,r) (zer,one,x) in F8 $ fillpx 8 z

------------------------------------------------------------

instance Eq GF256 where
    (==) = f8eq

instance Show GF256 where
    show = f8show

instance Parse GF256 where
    parse = f8parse

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

ex0 :: GF256
ex0 = parse "00"

ex1 :: GF256
ex1 = parse "0A"

ex2 :: GF256
ex2 = parse "09"

ex3 :: GF256
ex3 = parse "57"

ex4 :: GF256
ex4 = parse "13"

ex5 :: GF256
ex5 = parse "FE"

ex6 :: GF256
ex6 = parse "83"

mx :: GF256
mx = F8 $ parse "1 1 0 1 1 0 0 0 1"

------------------------------------------------------------

-- EOF
