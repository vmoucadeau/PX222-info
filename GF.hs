
module GF where

import Z_sur_nZ
import Group
import Field
import Ring
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

instance Ring GF2 where
    zero = GF2 0
    one = GF2 1
    opp = oppGF2
    add = addGF2
    mult = multGF2

-- instance Field GF2 where
--     inv = invGF2
-- Nope ! Ca c'était l'opposé, l'inverse va être bien plus galère à faire :D

-----------------------------------------------------------------
-- Définition de l'anneau des polynômes à coefficients         --
-----------------------------------------------------------------
-- Exemple : [1 0 1 0 1 0 0 1] = 1 + x² + x⁴ + x⁷
newtype Polynome = Pol [Integer] deriving (Show)

addPol :: Polynome -> Polynome -> Polynome
addPol (Pol a) (Pol b) = Pol (opList (+) 0 a b)

addzero :: Polynome -> Polynome
addzero (Pol a) = Pol (0:a)

oppPol :: Polynome -> Polynome
oppPol (Pol a) = Pol $ map (\x -> -x) a

multPol :: Polynome -> Polynome -> Polynome
multPol (Pol []) _ = Pol [0]
multPol _ (Pol []) = Pol [0]
multPol (Pol (x:xs)) (Pol b) = addPol (Pol (multScalaire (*) x b)) (addzero (multPol (Pol xs) (Pol b)))

addPol_modP :: Integer -> Polynome -> Polynome -> Polynome
addPol_modP p (Pol a) (Pol b) = Pol (opList (addModP p) 0 a b)

multPol_modP :: Integer -> Polynome -> Polynome -> Polynome
multPol_modP p (Pol []) _ = Pol [0]
multPol_modP p _ (Pol []) = Pol [0]
multPol_modP p (Pol (x:xs)) (Pol b) = addPol_modP p (Pol (multScalaire (multModP p) x b)) (addzero (multPol_modP p (Pol xs) (Pol b)))

show_pol :: Polynome -> String
show_pol (Pol (x1:x2:xs)) = "P(x) = " ++ show x1 ++ " + " ++ show x2 ++ "x + " ++ (intercalate " + " (zipWith (\c n -> show c ++ "x^" ++ show n) xs [2..length xs - 1]))

-- pol_mod_pol :: Polynome -> Polynome -> Polynome
-- pol_mod_pol (Pol a) (Pol b) = Pol(snd $ polyDiv a b)

instance Ring Polynome where
    zero = Pol [0]
    one = Pol [1]
    opp = oppPol
    add = addPol
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
    unit = GF [Z2Z 0]
    inverse = opposeGF
    operation = addGF
