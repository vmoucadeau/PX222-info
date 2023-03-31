
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

module GF where

import Struct
import Scalaire
import Data.List (intercalate)

------------------------------------------------------------------
-- Définition de GF2, instanciation dans les classes Ring/Field --
------------------------------------------------------------------
newtype GF2 a = GF2 a deriving (Show)

addGF2 :: Group a => GF2 a -> GF2 a -> GF2 a
addGF2 (GF2 a) (GF2 b) = GF2 $ add a b

oppGF2 :: Group a => GF2 a -> GF2 a
oppGF2 (GF2 a) = GF2 $ opp a

multGF2 :: Ring a => GF2 a -> GF2 a -> GF2 a
multGF2 (GF2 a) (GF2 b) = GF2 (mul a b)

invGF2 :: Field a => GF2 a -> GF2 a
invGF2 (GF2 a) = GF2 $ inv a

instance Group a => Group (GF2 a) where
    zer = GF2 (zer :: a)
    opp = oppGF2
    add = addGF2

instance Ring a => Ring (GF2 a) where
    one = GF2 (one :: a)
    mul = multGF2

instance Field a => Field (GF2 a) where
    inv = invGF2

-----------------------------------------------------------------
-- Définition de l'anneau des polynômes à coefficients         --
-----------------------------------------------------------------
-- Exemple : [1 0 1 0 1 0 0 1] = 1 + x² + x⁴ + x⁷
newtype Polynome a = Pol [a] deriving (Show)

addPol :: Num a => Polynome a -> Polynome a -> Polynome a
addPol (Pol a) (Pol b) = Pol (opList (+) 0 a b)

subPol :: Num a => Polynome a -> Polynome a -> Polynome a
subPol (Pol a) (Pol b) = Pol (opList (-) 0 a b)

addzero :: Num a => Polynome a -> Polynome a
addzero (Pol a) = Pol (0:a)

oppPol :: Num a => Polynome a -> Polynome a
oppPol (Pol a) = Pol $ map (\x -> -x) a

multPol :: Num a => Polynome a -> Polynome a -> Polynome a
multPol (Pol []) _ = Pol [0]
multPol _ (Pol []) = Pol [0]
multPol (Pol (x:xs)) (Pol b) = addPol (Pol (map (x *) b)) (addzero (multPol (Pol xs) (Pol b)))

-- degPol :: Num a => Polynome a -> Int
-- degPol (Pol [0]) = -1
-- degPol (Pol []) = -1
-- degPol (Pol list) = (length list) - 1

show_pol :: Polynome Float -> String
show_pol (Pol (x1:x2:xs)) = "P(x) = " ++ show x1 ++ " + " ++ show x2 ++ "x + " ++ intercalate " + " (zipWith (\c n -> show c ++ "x^" ++ show n) xs [2..length xs - 1])


tailPol :: Num a => Polynome a -> Polynome a
tailPol (Pol (x:xs)) = Pol (xs)

toList :: Num a => Polynome a -> [a]
toList (Pol list) = list

-- divPol :: Fractional a => Polynome a -> Polynome a -> Polynome a
-- divPol (Pol a) (Pol b) | degPol (Pol a) >= degPol (Pol b) = Pol (divPol' a b)
--                        | otherwise = Pol([0])
--     where divPol' (x:xs) y 

-- modPol :: Fractional a => Polynome a -> Polynome a -> Polynome a
-- modPol (Pol a) (Pol b) = Pol (reverse $ modPol' (reverse a) (reverse b))
--     where modPol' (x:xs) y | length (x:xs) < length y = []
--                            | otherwise = (x/head y) : toList (modPol ( (tailPol (subPol (Pol (x:xs)) (multPol (Pol [x/head y]) Pol(y))))  ))
--           modPol' [] _ = []

instance Group (Polynome Integer) where
    zer = Pol [0]
    opp = oppPol
    add = addPol

instance Ring (Polynome Integer) where
    one = Pol [1]
    mul = multPol

-- instance Show Polynome where
--     show = show_pol

-----------------------------------------------------------------
-- Définition de GF 256, instanciation dans la classe anneau (plus tard ?) --
-----------------------------------------------------------------
-- Exemple : [1 0 1 0 1 0 0 1] = 1 + x² + x⁴ + x⁷
-- Exemple d'addition [1 0 1 0 1 0 0 1] + [0 1 0 0 1 0 0 1] = [1 1 1 0 0 0 0 0] = 1 + x + x²

newtype GF256 = GF [Zs2Z] deriving (Show)

addGF ::  GF256 -> GF256 -> GF256
addGF (GF a) (GF b) = GF $ opList add (Z2Z 0) a b

opposeGF :: GF256 -> GF256
opposeGF (GF a) = GF $ map opp a

instance Group GF256 where
    zer = GF [Z2Z 0]
    opp = opposeGF
    add = addGF
