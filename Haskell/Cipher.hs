
module Cipher where

import Math.Struct
import Math.Poly
import Math.Bits
import Math.Byte
import Math.Word

newtype State = SQ (Poly GF4X)

nB :: Int
nB = 4

nK :: Int
nK = 4

nR :: Int
nR = 6 + nK

sqshow :: State -> String
sqshow x = let [x0,x1,x2,x3] = ranks x
    in show (W4 (Px x0)) ++ "\n" ++ show (W4 (Px x1)) ++ "\n" ++ show (W4 (Px x2)) ++ "\n" ++ show (W4 (Px x3))

sqparse :: State -> String -> State
sqparse _ x = let (Px z) = parse (Px [F8 zer]) x in build z

sqeq :: State -> State -> Bool
sqeq (SQ x) (SQ y) = x == y

sqzer :: State
sqzer = SQ $ fillpol nB zer

sqadd :: State -> State -> State
sqadd (SQ x) (SQ y) = SQ $ fillpol nB (add x y)

sqopp :: State -> State
sqopp (SQ x) = SQ $ fillpol nB (opp x)

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
-- -------- Cipher and Inverse Cipher prototypes -------- --
------------------------------------------------------------

encode :: [GF256] -> State -> State
encode k x = let key = keyexpansion k in rounds 1 key $ addroundkey 0 key x

rounds :: Int -> [GF4X] -> State -> State
rounds n k x | n == nR = addroundkey n k $ shiftrows $ subbytes x
rounds n k x = rounds (n+1) k $ morph n k x

morph :: Int -> [GF4X] -> State -> State
morph n k x = addroundkey n k $ mixcolumns $ shiftrows $ subbytes x

decode :: [GF256] -> State -> State
decode k x = let key = keyexpansion k in invrounds (nR-1) key $ addroundkey nR key x

invrounds :: Int -> [GF4X] -> State -> State
invrounds n k x | n == 0 = addroundkey n k $ invsubbytes $ invshiftrows x
invrounds n k x = invrounds (n-1) k $ invmorph n k x

invmorph :: Int -> [GF4X] -> State -> State
invmorph n k x = invmixcolumns $ addroundkey n k $ invsubbytes $ invshiftrows x

------------------------------------------------------------
-- -------- Individuals transformations of State -------- --
------------------------------------------------------------

subbytes :: State -> State
subbytes x = build $ map subbyte $ pixel x

shiftrows :: State -> State
shiftrows x = let [x0,x1,x2,x3] = ranks x in let
    [Px y0,Px y1,Px y2,Px y3] = [shiftrow 0 (Px x0),shiftrow 3 (Px x1),shiftrow 2 (Px x2),shiftrow 1 (Px x3)]
    in unranks [y0,y1,y2,y3]

mixcolumns :: State -> State
mixcolumns x = SQ $ Px $ map (\x -> mul ax (W4 $ Px x)) $ files x

addroundkey :: Int -> [GF4X] -> State -> State
addroundkey n k x = add x $ SQ $ Px $ take 4 $ drop (4*n) k

invsubbytes :: State -> State
invsubbytes x = build $ map invsubbyte $ pixel x

invshiftrows :: State -> State
invshiftrows x = let [x0,x1,x2,x3] = ranks x in let
    [Px y0,Px y1,Px y2,Px y3] = [shiftrow 0 (Px x0),shiftrow 1 (Px x1),shiftrow 2 (Px x2),shiftrow 3 (Px x3)]
    in unranks [y0,y1,y2,y3]

invmixcolumns :: State -> State
invmixcolumns x = SQ $ Px $ map (\x -> mul axi (W4 $ Px x)) $ files x

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

pxA4 :: Poly Zs2Z
pxA4 = Px [zer,one,zer,one,zer,zer,one,zer]

invsubbyte :: GF256 -> GF256
invsubbyte x = let (F8 ix) = subs x (F8 px63) in let (_,z) = die (mul pxA4 ix) px101 in inv $ F8 z

------------------------------------------------------------

px02 :: Ring a => Poly a
px02 = Px [zer,one]

powerxn :: Field a => Int -> Poly a
powerxn n = apply n (mul px02) one

shiftrow :: Int -> Poly GF256 -> Poly GF256
shiftrow n x = let (_,z) = die (mul x (powerxn n)) (add one (powerxn nB)) in fillpol nB z

------------------------------------------------------------

ax :: GF4X
ax = bpw4 "02 01 01 03"

axi :: GF4X
axi = bpw4 "0E 09 0D 0B"

------------------------------------------------------------

keyexpansion :: [GF256] -> [GF4X]
keyexpansion x = genkey nK $ initkey x

initkey :: [GF256] -> [GF4X]
initkey x = let SQ (Px key) = build x in key

subword :: GF4X -> GF4X
subword (W4 (Px x)) = W4 $ Px $ map subbyte x

rotword :: GF4X -> GF4X
rotword = mul (W4 $ powerxn 3)

rcon :: Int -> GF4X
rcon i = W4 $ fillpol 4 $ Px [apply (i-1) (mul (F8 px02)) one]

genkey :: Int -> [GF4X] -> [GF4X]
genkey n x | n == (nB * (nR + 1)) = x
genkey n x = genkey (n+1) (x ++ [genword n x])

genword :: Int -> [GF4X] -> GF4X
genword n x | mod n nK == 0 = add (x !! (n-nK)) $ add (rcon (div n nK)) $ subword $ rotword $ last x
genword n x | (&&) (nK == 8) (mod n nK == 4) = add (x !! (n-nK)) $ subword $ last x
genword n x = add (x !! (n-nK)) $ last x

------------------------------------------------------------
-- ---------- Nothing more than better parsing ---------- --
------------------------------------------------------------

bpz2z :: String -> Zs2Z
bpz2z = parse (Z2Z True)

bpf8 :: String -> GF256
bpf8 = parse (F8 zer)

bpw4 :: String -> GF4X
bpw4 = parse (W4 zer)

bpsq :: String -> State
bpsq = parse (SQ zer)

bppol :: Parse a => a -> String -> Poly a
bppol x = parse (Px [x])

bpkey :: String -> [GF256]
bpkey x = let (Px z) = parse (Px [F8 zer]) x in z

------------------------------------------------------------
-- --------- Some multiple random State Example --------- --
------------------------------------------------------------

rsq1 :: State
rsq1 = bpsq "70 26 AF DB 66 80 F2 A2 F9 6C 54 53 C0 69 87 95"

rsq2 :: State
rsq2 = bpsq "F7 E2 70 B4 9E 1E AF F0 B7 76 2B 3A 42 39 FF 52"

rsq3 :: State
rsq3 = bpsq "D5 DC F6 3B 21 52 81 D3 69 9E B3 B9 C0 35 31 12"

rsq5 :: State
rsq5 = bpsq "32 43 F6 A8 88 5A 30 8D 31 31 98 A2 E0 37 07 34"

rkey1 :: [GF256]
rkey1 = bpkey "2B 7E 15 16 28 AE D2 A6 AB F7 15 88 09 CF 4F 3C"

rkey2 :: [GF256]
rkey2 = bpkey "8E 73 B0 F7 DA 0E 64 52 C8 10 F3 2B 80 90 79 E5 62 F8 EA D2 52 2C 6B 7B"

rkey3 :: [GF256]
rkey3 = bpkey "60 3D EB 10 15 CA 71 BE 2B 73 AE F0 85 7D 77 81 1F 35 2C 07 3B 61 08 D7 2D 98 10 A3 09 14 DF F4"

------------------------------------------------------------
------------------------------------------------------------
------------------------------------------------------------

-- EOF
