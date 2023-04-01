
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}

module GF where

import Struct
import Scalaire
import Data.List (intercalate)
import Data.Fixed (mod', div')

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

addpol :: Num a => Polynome a -> Polynome a -> Polynome a
addpol (Pol a) (Pol b) = Pol (opList (+) 0 a b)

subpol :: Num a => Polynome a -> Polynome a -> Polynome a
subpol (Pol a) (Pol b) = Pol (opList (-) 0 a b)

addzero :: Num a => Polynome a -> Polynome a
addzero (Pol a) = Pol (0:a)

oppPol :: Num a => Polynome a -> Polynome a
oppPol (Pol a) = Pol $ map (\x -> -x) a

cleanpol :: Eq a => Num a => Polynome a -> Polynome a
cleanpol (Pol []) = (Pol [])
cleanpol (Pol list)  | last list == 0 = Pol (init list)
                     | otherwise = (Pol list)

multpol :: Eq a => Num a => Polynome a -> Polynome a -> Polynome a
multpol (Pol []) _ = Pol [0]
multpol _ (Pol []) = Pol [0]
multpol (Pol (x:xs)) (Pol b) = cleanpol (addpol (Pol (map (x *) b)) (addzero (multpol (Pol xs) (Pol b))))

degpol :: Eq a => Num a => Polynome a -> Int
degpol (Pol []) = -1
degpol (Pol (x:xs)) | x /= 0 = 1 + degpol (Pol xs)
                    | otherwise = 0 + degpol (Pol xs)

show_pol :: Polynome Float -> String
show_pol (Pol (x1:x2:xs)) = "P(x) = " ++ show x1 ++ " + " ++ show x2 ++ "x + " ++ intercalate " + " (zipWith (\c n -> show c ++ "x^" ++ show n) xs [2..length xs - 1])

toList :: Num a => Polynome a -> [a]
toList (Pol list) = list

divpol :: (Eq a, RealFrac a) => Polynome a -> Polynome a -> Polynome a 
divpol (Pol a) (Pol b) = cleanpol $ Pol (reverse $ divpol' (reverse a) (reverse b))
    where divpol' (x:xs) y | length (x:xs) == 1 && length y == 1 = [fromIntegral (truncate x `div` truncate (head y))] 
                           | length (x:xs) < length y = []
                           | otherwise = x/head y : (divpol' (tail(toList(subpol (Pol (x:xs)) (multpol (Pol [x/head y]) (Pol y))))) y)
          divpol' [] _ = []

modpol :: (Eq a, RealFrac a) => Polynome a -> Polynome a -> Polynome a
modpol (Pol a) (Pol b) = cleanpol $ Pol (reverse $ modpol' (reverse a) (reverse b))
    where modpol' (x:xs) y | length (x:xs) == 1 && length y == 1 = [fromIntegral (truncate x `mod` truncate (head y))]
                           | length (x:xs) < length y = (x:xs)
                           | otherwise = (modpol' (tail(toList(subpol (Pol (x:xs)) (multpol (Pol [x/head y]) (Pol y))))) y)
          modpol' [] _ = []

euclidepol :: (Eq a, RealFrac a) => Polynome a -> Polynome a -> (Polynome a, Polynome a, Polynome a)
euclidepol (Pol a) (Pol b) | b == [] = ((Pol a), Pol [1], Pol [0])
                           | otherwise = (d', v', (subpol (u') (multpol v' (divpol(Pol a) (Pol b))) ) )
                where (d', u', v') = euclidepol (Pol b) (modpol (Pol a) (Pol b))



instance Group (Polynome Integer) where
    zer = Pol [0]
    opp = oppPol
    add = addpol

instance Ring (Polynome Integer) where
    one = Pol [1]
    mul = multpol

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
