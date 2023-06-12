
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
nK = 8

nR :: Int
nR = 6 + nK

sqeq :: State -> State -> Bool
sqeq (SQ x) (SQ y) = (==) x y

sqshow :: State -> String
sqshow x = let [x0,x1,x2,x3] = ranks x
    in show (W4 $ Px x0) ++ show (W4 $ Px x1) ++ show (W4 $ Px x2) ++ show (W4 $ Px x3)
    -- in show (W4 $ Px x0) ++ "\n" ++ show (W4 $ Px x1) ++ "\n" ++ show (W4 $ Px x2) ++ "\n" ++ show (W4 $ Px x3)

sqparse :: String -> State
sqparse [a0,a1,a2,a3,a4,a5,a6,a7,b0,b1,b2,b3,b4,b5,b6,b7,c0,c1,c2,c3,c4,c5,c6,c7,d0,d1,d2,d3,d4,d5,d6,d7] = SQ $ Px $
    parse <$> [[a0,a1,a2,a3,a4,a5,a6,a7],[b0,b1,b2,b3,b4,b5,b6,b7],[c0,c1,c2,c3,c4,c5,c6,c7],[d0,d1,d2,d3,d4,d5,d6,d7]]
sqparse x = sqparse $ filter ((/=) ' ') x

sqzer :: State
sqzer = SQ $ fillpx nB zer

sqadd :: State -> State -> State
sqadd (SQ x) (SQ y) = SQ $ fillpx nB $ add x y

sqopp :: State -> State
sqopp (SQ x) = SQ $ fillpx nB x

------------------------------------------------------------

instance Eq State where
    (==) = sqeq

instance Show State where
    show = sqshow

instance Parse State where
    parse = sqparse

instance Group State where
    zer = sqzer
    add = sqadd
    opp = sqopp

------------------------------------------------------------
-- ------------- Useful state manipulations ------------- --
------------------------------------------------------------

pixel :: State -> [GF256]
pixel (SQ (Px [W4 (Px x)])) = x
pixel (SQ (Px ((W4 (Px x)):xs))) = x ++ (pixel (SQ (Px xs)))

build :: [GF256] -> State
build x = let
    sbuild [x0,x1,x2,x3] = [W4 $ Px [x0,x1,x2,x3]]
    sbuild (x0:x1:x2:x3:xs) = [W4 $ Px [x0,x1,x2,x3]] ++ sbuild xs
    in SQ $ Px $ sbuild x

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
-- ---------- Cipher and Inverse Cipher addons ---------- --
------------------------------------------------------------

encode :: String -> String -> String
encode k x | length x > 16 = (encodebloc k $ take 16 x) ++ (encode k $ drop 16 x)
encode k x | length x == 16 = encodebloc k x
encode k x = encode k (x ++ " ")

decode :: String -> String -> String
decode k x | length x > 16 = (decodebloc k $ take 16 x) ++ (decode k $ drop 16 x)
decode k x | length x == 16 = decodebloc k x
decode k x = decode k (x ++ " ")

encodebloc :: String -> String -> String
encodebloc k x = map invchartoGF $ pixel $ encodeciph (bpkey k) $ build $ map chartoGF x

decodebloc :: String -> String -> String
decodebloc k x = map invchartoGF $ pixel $ decodeciph (bpkey k) $ build $ map chartoGF x

------------------------------------------------------------
-- -------- Cipher and Inverse Cipher prototypes -------- --
------------------------------------------------------------

encodeciph :: [GF256] -> State -> State
encodeciph k x = let key = keyexpansion k in rounds 1 key $ addroundkey 0 key x

rounds :: Int -> [GF4X] -> State -> State
rounds n k x | n == nR = addroundkey n k $ shiftrows $ subbytes x
rounds n k x = rounds (n+1) k $ morph n k x

morph :: Int -> [GF4X] -> State -> State
morph n k x = addroundkey n k $ mixcolumns $ shiftrows $ subbytes x

decodeciph :: [GF256] -> State -> State
decodeciph k x = let key = keyexpansion k in invrounds (nR-1) key $ addroundkey nR key x

invrounds :: Int -> [GF4X] -> State -> State
invrounds n k x | n == 0 = addroundkey n k $ invsubbytes $ invshiftrows x
invrounds n k x = invrounds (n-1) k $ invmorph n k x

invmorph :: Int -> [GF4X] -> State -> State
invmorph n k x = invmixcolumns $ addroundkey n k $ invsubbytes $ invshiftrows x

------------------------------------------------------------
-- -------- Individuals transformations of State -------- --
------------------------------------------------------------

subbytes :: State -> State
subbytes x = build $ subbyte <$> pixel x

shiftrows :: State -> State
shiftrows x = let [x0,x1,x2,x3] = ranks x
    in unranks [shiftrow 0 x0,shiftrow 3 x1,shiftrow 2 x2,shiftrow 1 x3]

mixcolumns :: State -> State
mixcolumns x = SQ $ Px $ mul ax <$> W4 <$> Px <$> files x

addroundkey :: Int -> [GF4X] -> State -> State
addroundkey n k x = add x $ SQ $ Px $ take 4 $ drop (4*n) k

invsubbytes :: State -> State
invsubbytes x = build $ invsubbyte <$> pixel x

invshiftrows :: State -> State
invshiftrows x = let [x0,x1,x2,x3] = ranks x
    in unranks [shiftrow 0 x0,shiftrow 1 x1,shiftrow 2 x2,shiftrow 3 x3]

invmixcolumns :: State -> State
invmixcolumns x = SQ $ Px $ mul axi <$> W4 <$> Px <$> files x

------------------------------------------------------------
-- -------- Support functions to transformations -------- --
------------------------------------------------------------

-- subbytes

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

-- shiftrows

px02 :: Ring a => Poly a
px02 = Px [zer,one]

powerxn :: Field a => Int -> Poly a
powerxn n = apply n (mul px02) one

shiftrow :: Int -> [GF256] -> [GF256]
shiftrow n x = let (_,z) = die (mul (Px x) (powerxn n)) (add one (powerxn nB)) in let (Px y) = fillpx nB z in y

------------------------------------------------------------

-- mixcolumns

ax :: GF4X
ax = parse "02 01 01 03"

axi :: GF4X
axi = parse "0E 09 0D 0B"

------------------------------------------------------------

-- addroundkey

keyexpansion :: [GF256] -> [GF4X]
keyexpansion x = genkey nK $ initkey x

initkey :: [GF256] -> [GF4X]
initkey x = let SQ (Px key) = build x in key

subword :: GF4X -> GF4X
subword (W4 (Px x)) = W4 $ Px $ map subbyte x

rotword :: GF4X -> GF4X
rotword = mul (W4 $ powerxn 3)

rcon :: Int -> GF4X
rcon i = W4 $ fillpx 4 $ Px [apply (i-1) (mul (F8 px02)) one]

genkey :: Int -> [GF4X] -> [GF4X]
genkey n x | n == (nB * (nR + 1)) = x
genkey n x = genkey (n+1) (x ++ [genword n x])

genword :: Int -> [GF4X] -> GF4X
genword n x | mod n nK == 0 = add (x !! (n-nK)) $ add (rcon (div n nK)) $ subword $ rotword $ last x
genword n x | (&&) (nK == 8) (mod n nK == 4) = add (x !! (n-nK)) $ subword $ last x
genword n x = add (x !! (n-nK)) $ last x

------------------------------------------------------------
-- ----------- Simple tools to manage strings ----------- --
------------------------------------------------------------

stateprint :: State -> String
stateprint (SQ (Px x)) = let
    f [] = []
    f ((W4 (Px x)):xs) = x ++ f xs
    g (F8 (Px [x0,x1,x2,x3,x4,x5,x6,x7])) = hexaprint ([x7,x6,x5,x4] >>= show) ++ hexaprint ([x3,x2,x1,x0] >>= show)
    in f x >>= g

spacewords :: String -> String
spacewords [] = []
spacewords (x:y:l) = x:y:' ':(spacewords l)

bpkey :: String -> [GF256]
bpkey x = let (Px z) = parse $ spacewords $ filter ((/=) ' ') x in z

chartoGF :: Char -> GF256
chartoGF c = let x = fromEnum c in let
    f 0 = "0"
    f x = (if even x then '0' else '1'):(f $ div x 2)
    in F8 $ Px $ parse <$> return <$> take 8 (f x ++ "00000000")

invchartoGF :: GF256 -> Char
invchartoGF z = let b = let [x,y] = show z in hexaparse [x] ++ hexaparse [y] in let
    f [] = 0
    f (c:cs) = (if c == '0' then 0 else 1) + 2 * (f cs)
    in toEnum $ f $ reverse b

------------------------------------------------------------
-- --------- Some multiple random State Example --------- --
------------------------------------------------------------

rsq1 :: State
rsq1 = parse "70 26 AF DB 66 80 F2 A2 F9 6C 54 53 C0 69 87 95"

rsq2 :: State
rsq2 = parse "F7 E2 70 B4 9E 1E AF F0 B7 76 2B 3A 42 39 FF 52"

rsq3 :: State
rsq3 = parse "D5 DC F6 3B 21 52 81 D3 69 9E B3 B9 C0 35 31 12"

rsq5 :: State
rsq5 = parse "32 43 F6 A8 88 5A 30 8D 31 31 98 A2 E0 37 07 34"

rkey1 :: [GF256]
rkey1 = bpkey "2B 7E 15 16 28 AE D2 A6 AB F7 15 88 09 CF 4F 3C"

rkey2 :: [GF256]
rkey2 = bpkey "8E 73 B0 F7 DA 0E 64 52 C8 10 F3 2B 80 90 79 E5 62 F8 EA D2 52 2C 6B 7B"

rkey3 :: [GF256]
rkey3 = bpkey "60 3D EB 10 15 CA 71 BE 2B 73 AE F0 85 7D 77 81 1F 35 2C 07 3B 61 08 D7 2D 98 10 A3 09 14 DF F4"

txtsq1 :: String
txtsq1 = "7026AFDB6680F2A2F96C5453C0698795"

txtsq2 :: String
txtsq2 = "F7E270B49E1EAFF0B7762B3A4239FF52"

txtsq3 :: String
txtsq3 = "D5DCF63B215281D3699EB3B9C0353112"

txtsq5 :: String
txtsq5 = "3243F6A8885A308D313198A2E0370734"

txtkey1 :: String
txtkey1 = "2B7E151628AED2A6ABF7158809CF4F3C"

txtkey2 :: String
txtkey2 = "8E73B0F7DA0E6452C810F32B809079E562F8EAD2522C6B7B"

txtkey3 :: String
txtkey3 = "603DEB1015CA71BE2B73AEF0857D77811F352C073B6108D72D9810A30914DFF4"

------------------------------------------------------------

-- EOF
