module GF where

    import Z_sur_nZ
    import Group
    import Field
    import Ring
    
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

    -----------------------------------------------------------------
    -- Fonctions génériques de calcul modulo p --
    -----------------------------------------------------------------

    -- multiplication modulo p --
    multModP :: Integer -> Integer -> Integer -> Integer
    multModP p n m = (n*m) `mod` p


    ------------------------------------------------------------------
    -- Définition de GF2, instanciation dans les classes Ring/Field --
    ------------------------------------------------------------------
    newtype GF2 = GF2 Integer deriving (Show)

    addGF2 ::  GF2 -> GF2 -> GF2
    addGF2 (GF2 a) (GF2 b) = GF2 $ addModP 2 a b

    invGF2 :: GF2 -> GF2
    invGF2 (GF2 a) = GF2 $ oppose 2 a

    multGF2 :: GF2 -> GF2 -> GF2
    multGF2 (GF2 a) (GF2 b) = GF2 (multModP 2 a b)

    instance Ring GF2 where
        zero = GF2 0
        one = GF2 1
        add = addGF2
        mult = multGF2
    
    instance Field GF2 where
        inv = invGF2


    -----------------------------------------------------------------
    -- Définition de l'anneau des polynômes à coefficients         --
    -----------------------------------------------------------------
    -- Exemple : [1 0 1 0 1 0 0 1] = 1 + x² + x⁴ + x⁷
    newtype Polynome = Pol [Integer] deriving (Show)

    addPol :: Polynome -> Polynome -> Polynome
    addPol (Pol a) (Pol b) = Pol (opList (+) 0 a b)
    
    addzero :: Polynome -> Polynome
    addzero (Pol a) = Pol (0:a)

    multPol :: Polynome -> Polynome -> Polynome
    multPol (Pol []) _ = Pol [0]
    multPol _ (Pol []) = Pol [0]
    multPol (Pol (x:xs)) (Pol b) = addPol (Pol (multScalaire (*) x b)) (addzero (multPol (Pol xs) (Pol b)))

    -----------------------------------------------------------------
    -- Définition de GF 256, instanciation dans la classe groupe --
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