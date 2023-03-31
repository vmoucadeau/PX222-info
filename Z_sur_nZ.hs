
module Z_sur_nZ where

import Struct
---------------------------------------------
-- Fonctions génériques de calcul modulo p --
---------------------------------------------

-- addition modulo p
addModP :: Integer -> Integer -> Integer -> Integer
addModP p n m = (n+m) `mod` p 

-- opposé modulo p
opposeModP :: Integer -> Integer -> Integer
opposeModP p n | n == 0 = 0 | otherwise = p - n

-- multiplication modulo p --
multModP :: Integer -> Integer -> Integer -> Integer
multModP p n m = (n*m) `mod` p

-----------------------------------------------------------------
-- Définition de Z sur 2Z, instanciation dans la classe groupe --
-----------------------------------------------------------------

newtype Z_sur_2Z = Z2Z Integer deriving (Show)

addMod2 :: Z_sur_2Z-> Z_sur_2Z -> Z_sur_2Z
addMod2 (Z2Z a) (Z2Z b) = Z2Z $ addModP 2 a b

oppose2 :: Z_sur_2Z -> Z_sur_2Z
oppose2 (Z2Z n) = Z2Z $ opposeModP 2 n

instance Group Z_sur_2Z where
  zero = Z2Z 0
  opp = oppose2
  add = addMod2

-----------------------------------------------------------------
-- Définition de Z sur 5Z, instanciation dans la classe groupe --
-----------------------------------------------------------------

newtype Z_sur_5Z = Z5Z Integer    deriving (Show)

addMod5 :: Z_sur_5Z-> Z_sur_5Z -> Z_sur_5Z
addMod5 (Z5Z a) (Z5Z b) = Z5Z $ addModP 5 a b

oppose5 :: Z_sur_5Z -> Z_sur_5Z
oppose5 (Z5Z n) = Z5Z $ opposeModP 5 n

instance Group Z_sur_5Z where
  zero = Z5Z 0
  opp = oppose5
  add = addMod5

-----------------------------------------------------------------
-- Définition de Z sur 6Z, instanciation dans la classe groupe --
-----------------------------------------------------------------

newtype Z_sur_6Z = Z6Z Integer    deriving (Show)

addMod6 :: Z_sur_6Z-> Z_sur_6Z -> Z_sur_6Z
addMod6 (Z6Z a) (Z6Z b) = Z6Z $ addModP 6 a b

oppose6 :: Z_sur_6Z -> Z_sur_6Z
oppose6 (Z6Z n) = Z6Z $ opposeModP 6 n

instance Group Z_sur_6Z where
  zero = Z6Z 0
  opp = oppose6
  add = addMod6
