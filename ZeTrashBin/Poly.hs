
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Poly where

import Math.Struct
-- import Math.Scalar
import Data.List (intercalate)


opList :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y:opList f n xs ys

------------------------------------------------------------------
-- Définition de GF2, truc parfaitement inutile vu qu'on a Z/2Z --
------------------------------------------------------------------

newtype GF2 a = GF2 a deriving (Show)

addGF2 :: Group a => GF2 a -> GF2 a -> GF2 a
addGF2 (GF2 a) (GF2 b) = GF2 $ add a b

oppGF2 :: Group a => GF2 a -> GF2 a
oppGF2 (GF2 a) = GF2 $ opp a

multGF2 :: Ring a => GF2 a -> GF2 a -> GF2 a
multGF2 (GF2 a) (GF2 b) = GF2 $ mul a b

invGF2 :: Field a => GF2 a -> GF2 a
invGF2 (GF2 a) = GF2 $ inv a

instance Group a => Group (GF2 a) where
    zer = GF2 zer
    opp = oppGF2
    add = addGF2

instance Ring a => Ring (GF2 a) where
    one = GF2 one
    mul = multGF2

instance Field a => Field (GF2 a) where
    inv = invGF2

-----------------------------------------------------------------
-- Définition de l'anneau des polynômes à coefficients         --
-----------------------------------------------------------------
-- Exemple : [1 0 1 0 1 0 0 1] = 1 + x² + x⁴ + x⁷
newtype Polynome a = Pol [a] deriving (Show)


addpol :: Group a => Polynome a -> Polynome a -> Polynome a
addpol (Pol a) (Pol b) = Pol (opList add zer a b)

subpol :: Group a => Polynome a -> Polynome a -> Polynome a
subpol (Pol a) (Pol b) = Pol (opList subs zer a b)

addzero :: Group a => Polynome a -> Polynome a
addzero (Pol a) = Pol (zer:a)

oppPol :: Group a => Polynome a -> Polynome a
oppPol (Pol a) = Pol $ map opp a

cleanpol :: (Group a, Eq a) => Polynome a -> Polynome a
cleanpol (Pol []) = (Pol [])
cleanpol (Pol list)  | last list == zer = cleanpol (Pol (init list))
                     | otherwise = (Pol list)

multpol :: (Ring a, Eq a) => Polynome a -> Polynome a -> Polynome a
multpol (Pol []) _ = Pol [zer]
multpol _ (Pol []) = Pol [zer]
multpol (Pol (x:xs)) (Pol b) = cleanpol (addpol (Pol (map (mul x) b)) (addzero (multpol (Pol xs) (Pol b))))

degpol :: (Ring a, Eq a) => Polynome a -> Int
degpol (Pol []) = -1
degpol (Pol x) | last x == zer = degpol $ cleanpol (Pol x) | otherwise = (length x)-1

show_pol :: Show a => Polynome a -> String
show_pol (Pol x) = "P(x) = (" ++ intercalate " + (" (zipWith (\c n -> show c ++ ")x^" ++ show n) x [0..length x + 1])

toList :: Polynome a -> [a]
toList (Pol list) = list

divpol :: (Ring a, Eq a, RealFrac a) => Polynome a -> Polynome a -> Polynome a
divpol (Pol a) (Pol b) = cleanpol $ Pol (reverse $ divpol' (reverse a) (reverse b))
    where divpol' (x:xs) y | length (x:xs) == 1 && length y == 1 = [fromIntegral (truncate x `div` truncate (head y))]
                           | length (x:xs) < length y = []
                           | otherwise = x/head y : (divpol' (tail(toList(subpol (Pol (x:xs)) (multpol (Pol [x/head y]) (Pol y))))) y)
          divpol' [] _ = []

modpol :: (Ring a, Eq a, RealFrac a) => Polynome a -> Polynome a -> Polynome a
modpol (Pol a) (Pol b) = cleanpol $ Pol (reverse $ modpol' (reverse a) (reverse b))
    where modpol' (x:xs) y | length (x:xs) == 1 && length y == 1 = [fromIntegral (truncate x `mod` truncate (head y))]
                           | length (x:xs) < length y = (x:xs)
                           | otherwise = (modpol' (tail(toList(subpol (Pol (x:xs)) (multpol (Pol [x/head y]) (Pol y))))) y)
          modpol' [] _ = []

euclidepol :: (Ring a, Eq a, RealFrac a) => Polynome a -> Polynome a -> (Polynome a, Polynome a, Polynome a)
euclidepol (Pol a) (Pol b) | b == [] = ((Pol a), Pol [1], Pol [0])
                           | otherwise = (d', v', (subpol (u') (multpol v' (divpol(Pol a) (Pol b))) ) )
                where (d', u', v') = euclidepol (Pol b) (modpol (Pol a) (Pol b))

-- instance Show a => Show (Polynome a) where
--     show = show_pol

instance Group Integer where
    zer = 0
    opp = (\x -> -x)
    add = (+)

instance Ring Integer where
    one = 1
    mul = (*)

-- instance Group a => Group (Polynome a) where
--     zer = Pol [zer]
--     opp = oppPol
--     add = addpol

-- instance (Ring a, Eq a) => Ring (Polynome a) where
--     one = Pol [one]
--     mul = multpol
