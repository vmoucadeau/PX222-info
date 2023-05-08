
module Cipher where

import Math.Struct
import Math.Poly
import Math.Bits
import Math.Byte
import Math.Word

newtype State = SQ (Poly GF4X)

nK :: Int
nK = 6

sqshow :: State -> String
sqshow x = let [x0,x1,x2,x3] = ranks x
    in show (Px x0) ++ "\n" ++ show (Px x1) ++ "\n" ++ show (Px x2) ++ "\n" ++ show (Px x3)

sqparse :: State -> String -> State
sqparse _ x = let (Px z) = parse (Px [F8 zer]) x in revbuild z

sqeq :: State -> State -> Bool
sqeq (SQ x) (SQ y) = x == y

sqzer :: State
sqzer = SQ $ fillpol nK zer

sqadd :: State -> State -> State
sqadd (SQ x) (SQ y) = SQ $ fillpol nK (add x y)

sqopp :: State -> State
sqopp (SQ x) = SQ $ fillpol nK (opp x)

instance Show State where
    show = sqshow

instance Parse State where
    parse = sqparse

instance Eq State where
    (==) = sqeq

instance Group State where
    zer = sqzer
    add = sqadd
    opp = sqopp

------------------------------------------------------------
-- -------- Individuals transformations of State -------- --
------------------------------------------------------------

subbytes :: State -> State
subbytes x = build $ map subbyte $ pixel x

shiftrows :: State -> State
shiftrows x = let [x0,x1,x2,x3] = ranks x in let
    [Px y0,Px y1,Px y2,Px y3] = [shiftrow 0 (Px x0),shiftrow 1 (Px x1),shiftrow 2 (Px x2),shiftrow 3 (Px x3)]
    in unranks [y0,y1,y2,y3]

mixcolumns :: State -> State
mixcolumns x = SQ $ Px $ map (\x -> mul ax (W4 $ Px x)) $ files x

addroundkey :: a -> State -> State
addroundkey k x = x

------------------------------------------------------------
-- ------------- Useful state manipulations ------------- --
------------------------------------------------------------

pixel :: State -> [GF256]
pixel (SQ (Px [W4 (Px x)])) = x
pixel (SQ (Px ((W4 (Px x)):xs))) = x ++ (pixel (SQ (Px xs)))

build :: [GF256] -> State
build x = let
    build1 [x0,x1,x2,x3] = [W4 $ Px [x0,x1,x2,x3]]
    build1 (x0:x1:x2:x3:xs) = [W4 $ Px [x0,x1,x2,x3]] ++ build1 xs
    in SQ $ Px $ build1 x

files :: State -> [[GF256]]
files x = let
    file [x0,x1,x2,x3] = return [x0,x1,x2,x3]
    file (x0:x1:x2:x3:xs) = [x0,x1,x2,x3]:file xs
    in file $ pixel x

ranks :: State -> [[GF256]]
ranks x = let
    rank [l0,l1,l2,l3] [x0,x1,x2,x3] = [l0 ++ [x0],l1 ++ [x1],l2 ++ [x2],l3 ++ [x3]]
    rank [l0,l1,l2,l3] (x0:x1:x2:x3:xs) = rank [l0 ++ [x0],l1 ++ [x1],l2 ++ [x2],l3 ++ [x3]] xs
    in rank [[],[],[],[]] $ pixel x

unranks :: [[GF256]] -> State
unranks x = let
    unrank [[x0],[x1],[x2],[x3]] = [x0,x1,x2,x3]
    unrank [(x0:x0s),(x1:x1s),(x2:x2s),(x3:x3s)] = [x0,x1,x2,x3] ++ unrank [x0s,x1s,x2s,x3s]
    in build $ unrank x

------------------------------------------------------------
-- -------- Support functions to transformations -------- --
------------------------------------------------------------

px1F :: Poly Zs2Z
px1F = Px [one,one,one,one,one,zer,zer,zer]

px101 :: Poly Zs2Z
px101 = Px [one,zer,zer,zer,zer,zer,zer,zer,one]

px63 :: Poly Zs2Z
px63 = Px [one,one,zer,zer,zer,one,one,zer]

subbyte :: GF256 -> GF256
subbyte x = let (F8 ix) = inv x in let (_,z) = die (mul px1F ix) px101 in F8 (add z px63)

------------------------------------------------------------

px02 :: Poly GF256
px02 = Px [zer,one]

powerxn :: Int -> Poly GF256
powerxn n = apply n (mul px02) one

shiftrow :: Int -> Poly GF256 -> Poly GF256
shiftrow n x = let (_,z) = die (mul x (powerxn n)) (add one (powerxn nK)) in fillpol nK z

------------------------------------------------------------

ax :: GF4X
ax = W4 $ Px [F8 $ fillpol 8 (Px [one,one]),one,one,F8 $ fillpol 8 (Px [zer,one])]

------------------------------------------------------------

addroundkey2 :: a -> State -> State
addroundkey2 k x = x

------------------------------------------------------------
-- ---------- Nothing more than better parsing ---------- --
------------------------------------------------------------

revbuild :: [GF256] -> State
revbuild x = let
    build2 [x0,x1,x2,x3] = [W4 $ Px $ reverse [x0,x1,x2,x3]]
    build2 (x0:x1:x2:x3:xs) = [W4 $ Px $ reverse [x0,x1,x2,x3]] ++ build2 xs
    in SQ $ Px $ build2 x

bpf8 :: String -> GF256
bpf8 = parse (F8 zer)

bpw4 :: String -> GF4X
bpw4 = parse (W4 zer)

bpsq :: String -> State
bpsq = parse (SQ zer)

bppol :: Parse a => a -> String -> Poly a
bppol x = parse (Px [x])

------------------------------------------------------------
-- --------- Some multiple random State Example --------- --
------------------------------------------------------------

rsq1 :: State
rsq1 = bpsq "70 26 AF DB 66 80 F2 A2 F9 6C 54 53 C0 69 87 95"

rsq2 :: State
rsq2 = bpsq "F7 E2 70 B4 9E C8 D5 E8 8D 1E AF F0 B7 09 92 C0 F7 76 2B 3A 42 39 FF 52"

rsq3 :: State
rsq3 = bpsq "77 D5 C1 DC 4D F6 0F 3B FE 21 2F 52 13 81 E8 D3 09 69 77 9E EF B3 69 B9 86 C0 76 35 95 31 D7 12"

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

-- EOF
