
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module GF where

import Z_sur_nZ
import Struct
import Data.List (intercalate) -- What is that ?

-----------------------------------------------------------------
-- Fonctions génériques de calcul --
-----------------------------------------------------------------

opList :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = (f x y):(opList f n xs ys)

multScalaire :: (a -> a -> a) -> a -> [a] -> [a]
multScalaire _ _ [] = []
multScalaire f n (x:xs) = (f n x):(multScalaire f n xs)

moduloList :: Integer -> [Integer] -> [Integer]
moduloList _ [] = []
moduloList n (x:xs) = [(x `mod` n)] ++ moduloList n xs

------------------------------------------------------------------
-- Définition de GF2, instanciation dans les classes Ring/Field --
------------------------------------------------------------------
newtype GF2 = GF2 Integer deriving (Show)

addGF2 ::  GF2 -> GF2 -> GF2
addGF2 (GF2 a) (GF2 b) = GF2 $ addModP 2 a b

oppGF2 :: GF2 -> GF2
oppGF2 (GF2 a) = GF2 $ opposeModP 2 a

multGF2 :: GF2 -> GF2 -> GF2
multGF2 (GF2 a) (GF2 b) = GF2 (multModP 2 a b)

instance Group GF2 where
    zero = GF2 0
    opp = oppGF2
    add = addGF2

instance Ring GF2 where
    one = GF2 1
    mult = multGF2

-- instance Field GF2 where
--     inv = invGF2
-- Nope ! Ca c'était l'opposé, l'inverse va être bien plus galère à faire :D

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
multPol (Pol (x:xs)) (Pol b) = addPol (Pol (multScalaire (*) x b)) (addzero (multPol (Pol xs) (Pol b)))



show_pol :: Polynome Float -> String
show_pol (Pol (x1:x2:xs)) = "P(x) = " ++ show x1 ++ " + " ++ show x2 ++ "x + " ++ intercalate " + " (zipWith (\c n -> show c ++ "x^" ++ show n) xs [2..length xs - 1])


modPol :: Fractional a => Polynome a -> Polynome a -> Polynome a
modPol (Pol a) (Pol b) = Pol (reverse $ modPol' (reverse a) (reverse b))
    where modPol' (x:xs) y | length (x:xs) < length y = []
                           | otherwise = (x/head y) : modPol ( (tail (subPol (x:xs) multPol [x/head y] y)))


instance Group (Polynome Integer) where
    zero = Pol [0]
    opp = oppPol
    add = addPol

instance Ring (Polynome Integer) where
    one = Pol [1]
    mult = multPol

-- instance Show Polynome where
--     show = show_pol

-----------------------------------------------------------------
-- Définition de GF 256, instanciation dans la classe anneau (plus tard ?) --
-----------------------------------------------------------------
-- Exemple : [1 0 1 0 1 0 0 1] = 1 + x² + x⁴ + x⁷
-- Exemple d'addition [1 0 1 0 1 0 0 1] + [0 1 0 0 1 0 0 1] = [1 1 1 0 0 0 0 0] = 1 + x + x²
newtype GF256 = GF [Z_sur_2Z] deriving (Show)

addGF ::  GF256 -> GF256 -> GF256
addGF (GF a) (GF b) = GF $ opList addMod2 (Z2Z 0) a b

opposeGF :: GF256 -> GF256
opposeGF (GF a) = GF $ map oppose2 a

instance Group GF256 where
    zero = GF [Z2Z 0]
    opp = opposeGF
    add = addGF
