
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Cipher where

import OldMath.Word

import OldMath.Byte
import OldMath.Scalar
import OldMath.Struct

------------------------------------------------------------
-- ------- Définition du State en tableau de mots ------- --
------------------------------------------------------------

newtype State = SQ [GF4X]

sqshow :: State -> String
sqshow (SQ x) = x >>= show

sqeq :: State -> State -> Bool
sqeq (SQ x) (SQ y) = foldl (&&) True (opList (==) zer x y)

instance Show State where
    show = sqshow

instance Eq State where
    (==) = sqeq

------------------------------------------------------------
-- -------- Définition des opérations dans State -------- --
------------------------------------------------------------

subbytes :: State -> State
subbytes (SQ []) = SQ []
subbytes (SQ ((W4 [x0,x1,x2,x3]):xs)) = let (SQ z) = subbytes (SQ xs) in SQ ((W4 [sub1 x0,sub1 x1,sub1 x2,sub1 x3]):z)

shiftrows :: State -> State
shiftrows = shiftrows_aux 3
    where shiftrows_aux n state | n == 0 = state
                                | otherwise = shiftrows_aux (n-1) $ set_row n (shift_cycle n (get_row n state) ) state

mixcolumns :: State -> State
mixcolumns (SQ []) = SQ []
mixcolumns (SQ([w1,w2,w3,w4])) = SQ([w4mul w1 ax,w4mul w2 ax,w4mul w3 ax,w4mul w4 ax])
mixcolumns _ = error "mixcolumns: state not of size 4"

addroundkey :: a -> b -> State -> State
addroundkey k1 k2 x = x

------------------------------------------------------------
-- ----------- Polynômes d'exemple dans State ----------- --
------------------------------------------------------------

st1 :: State
st1 = SQ [W4 [F8 [one,zer]], W4 [F8 [one,zer]], W4 [F8 [one,zer]], W4 [F8 [one,zer]]]

------------------------------------------------------------
-- ---------- Fonctions auxiliaires pour GF4X ----------- --
------------------------------------------------------------

opList :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y:opList f n xs ys

sub1 :: GF256 -> GF256
sub1 (F8 [x0,x1,x2,x3,x4,x5,x6,x7]) = let
    z0 = add (add (add x4 x5) (add x6 x7)) (add x0 one)
    z1 = add (add (add x5 x6) (add x7 x0)) (add x1 one)
    z2 = add (add (add x6 x7) (add x0 x1)) (add x2 zer)
    z3 = add (add (add x7 x0) (add x1 x2)) (add x3 zer)
    z4 = add (add (add x0 x1) (add x2 x3)) (add x4 zer)
    z5 = add (add (add x1 x2) (add x3 x4)) (add x5 one)
    z6 = add (add (add x2 x3) (add x4 x5)) (add x6 one)
    z7 = add (add (add x3 x4) (add x5 x6)) (add x7 zer)
    in F8 [z0,z1,z2,z3,z4,z5,z6,z7]

shift_cycle :: Int -> [GF256] -> [GF256]
shift_cycle 0 list = list
shift_cycle n (x:xs) = shift_cycle (n-1) (xs ++ [x])  

get_row :: Int -> State -> [GF256]
get_row n (SQ ([])) = []
get_row n (SQ ((W4([x0,x1,x2,x3])):xs)) | n == 0 = (x0 : (get_row n (SQ (xs))))
                                        | n == 1 = (x1 : (get_row n (SQ (xs))))
                                        | n == 2 = (x2 : (get_row n (SQ (xs))))
                                        | otherwise = (x3 : (get_row n (SQ (xs))))

set_row :: Int -> [GF256] -> State -> State
set_row n [] _ = SQ ([])
set_row n (polx:polxs) (SQ ((W4([x0,x1,x2,x3])):xs)) | n == 0 = SQ((W4 [polx,x1,x2,x3]) : (to_list $ set_row n polxs (SQ xs)))
                                                     | n == 1 = SQ((W4 [x0,polx,x2,x3]) : (to_list $ set_row n polxs (SQ xs)))
                                                     | n == 2 = SQ((W4 [x0,x1,polx,x3]) : (to_list $ set_row n polxs (SQ xs)))
                                                     | otherwise = SQ((W4 [x0,x1,x2,polx]) : (to_list $ set_row n polxs (SQ xs)))
    where to_list (SQ sqlist) = sqlist


empty :: a -> a
empty x = x

-- EOF
