
module Math.Word(GF4X(W4)) where

import Math.Struct
import Math.Poly
import Math.Byte

newtype GF4X = W4 (Poly GF256)

w4show :: GF4X -> String
w4show (W4 x) = show x

w4parse :: GF4X -> String -> GF4X
w4parse _ x = W4 $ fillpol 4 $ parse (Px [F8 zer]) x

w4eq :: GF4X -> GF4X -> Bool
w4eq (W4 x) (W4 y) = x == y

w4zer :: GF4X
w4zer = W4 $ fillpol 4 zer

w4add :: GF4X -> GF4X -> GF4X
w4add (W4 x) (W4 y) = W4 $ fillpol 4 (add x y)

w4opp :: GF4X -> GF4X
w4opp (W4 x) = W4 $ fillpol 4 (opp x)

w4one :: GF4X
w4one = W4 $ fillpol 4 one

w4mul :: GF4X -> GF4X -> GF4X
w4mul (W4 x) (W4 y) = let (_,W4 z) = die (W4 (mul x y)) mx in W4 $ fillpol 4 z

w4die :: GF4X -> GF4X -> (GF4X,GF4X)
w4die (W4 x) (W4 y) = let (q,r) = die x y in (W4 q, W4 r)

instance Show GF4X where
    show = w4show

instance Parse GF4X where
    parse = w4parse

instance Eq GF4X where
    (==) = w4eq

instance Group GF4X where
    zer = w4zer
    add = w4add
    opp = w4opp

instance Ring GF4X where
    one = w4one
    mul = w4mul
    die = w4die

------------------------------------------------------------
-- ------------ Example polynomials in GF4X ------------- --
------------------------------------------------------------

ex0 :: GF4X
ex0 = W4 $ Px []

ex1 :: GF4X
ex1 = W4 $ Px [zer,one,one,zer]

mx :: GF4X
mx = W4 $ Px [one,zer,zer,zer,one]

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

-- EOF
