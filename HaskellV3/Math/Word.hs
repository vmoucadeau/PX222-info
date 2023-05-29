
module Math.Word(GF4X(W4)) where

import Math.Struct
import Math.Poly
import Math.Byte

newtype GF4X = W4 (Poly GF256)

w4eq :: GF4X -> GF4X -> Bool
w4eq (W4 x) (W4 y) = (==) x y

w4show :: GF4X -> String
w4show (W4 (Px (x0:x1:x2:x3:xs))) | degpx (Px (x0:x1:x2:x3:xs)) > 3
    = (++) ("{" ++ w4show (W4 $ Px xs) ++ "}") (w4show (W4 $ Px (x0:x1:x2:x3:[])))
w4show (W4 (Px (x0:x1:x2:x3:[])))
    = "[" ++ ([x0,x1,x2,x3] >>= show) ++ "]"
w4show (W4 x) = w4show (W4 $ fillpx 4 x)

w4parse :: String -> GF4X
w4parse [x0,x1,x2,x3,x4,x5,x6,x7] = W4 $ Px $ parse <$> [[x0,x1],[x2,x3],[x4,x5],[x6,x7]]
w4parse x = w4parse $ filter ((/=) ' ') x

w4zer :: GF4X
w4zer = W4 $ fillpx 4 zer

w4add :: GF4X -> GF4X -> GF4X
w4add (W4 x) (W4 y) = W4 $ fillpx 4 $ add x y

w4opp :: GF4X -> GF4X
w4opp (W4 x) = W4 $ fillpx 4 x

w4one :: GF4X
w4one = W4 $ fillpx 4 one

w4mul :: GF4X -> GF4X -> GF4X
w4mul (W4 x) (W4 y) = let (_,W4 z) = die (W4 $ mul x y) mx in W4 $ fillpx 4 z

w4die :: GF4X -> GF4X -> (GF4X,GF4X)
w4die (W4 x) (W4 y) = let (q,r) = die x y in (W4 q, W4 r)

------------------------------------------------------------

instance Eq GF4X where
    (==) = w4eq

instance Show GF4X where
    show = w4show

instance Parse GF4X where
    parse = w4parse

instance Group GF4X where
    zer = w4zer
    add = w4add
    opp = w4opp

instance Ring GF4X where
    one = w4one
    mul = w4mul
    die = w4die

------------------------------------------------------------

ex0 :: GF4X
ex0 = parse "00000000"

ex1 :: GF4X
ex1 = parse "02010103"

ex2 :: GF4X
ex2 = parse "0E090D0B"

mx :: GF4X
mx = W4 $ parse "01 00 00 00 01"

------------------------------------------------------------

-- EOF
