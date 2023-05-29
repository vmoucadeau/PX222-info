
module Math.Struct where

class (Eq a,Show a) => Parse a where
    parse :: String -> a

class Parse a => Group a where
    zer :: a
    add :: a -> a -> a
    opp :: a -> a
    subs :: a -> a -> a
    subs x y = add x $ opp y

class Group a => Ring a where
    one :: a
    mul :: a -> a -> a
    die :: a -> a -> (a,a)

class Ring a => Field a where
    inv :: a -> a
    divs :: a -> a -> a
    divs x y = mul x $ inv y

------------------------------------------------------------

apply :: Int -> (a -> a) -> a -> a
apply 0 _ x = x
apply n f x = apply (n-1) f (f x)

opList :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
opList _ _ [] [] = []
opList f n [] x = opList f n [n] x
opList f n x [] = opList f n x [n]
opList f n (x:xs) (y:ys) = f x y : opList f n xs ys

hexaprint :: String -> String
hexaprint "0000" = "0"
hexaprint "0001" = "1"
hexaprint "0011" = "3"
hexaprint "0010" = "2"
hexaprint "0110" = "6"
hexaprint "0111" = "7"
hexaprint "0101" = "5"
hexaprint "0100" = "4"
hexaprint "1100" = "C"
hexaprint "1101" = "D"
hexaprint "1111" = "F"
hexaprint "1110" = "E"
hexaprint "1010" = "A"
hexaprint "1011" = "B"
hexaprint "1001" = "9"
hexaprint "1000" = "8"

hexaparse :: String -> String
hexaparse "0" = "0000"
hexaparse "1" = "0001"
hexaparse "3" = "0011"
hexaparse "2" = "0010"
hexaparse "6" = "0110"
hexaparse "7" = "0111"
hexaparse "5" = "0101"
hexaparse "4" = "0100"
hexaparse "C" = "1100"
hexaparse "D" = "1101"
hexaparse "F" = "1111"
hexaparse "E" = "1110"
hexaparse "A" = "1010"
hexaparse "B" = "1011"
hexaparse "9" = "1001"
hexaparse "8" = "1000"
hexaparse "c" = "1100"
hexaparse "d" = "1101"
hexaparse "f" = "1111"
hexaparse "e" = "1110"
hexaparse "a" = "1010"
hexaparse "b" = "1011"

------------------------------------------------------------

-- EOF
