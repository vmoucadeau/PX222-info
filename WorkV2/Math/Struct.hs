
module Math.Struct where

class (Show a,Eq a) => Group a where
    zer :: a
    add :: a -> a -> a
    opp :: a -> a
    subs :: a -> a -> a
    subs x y = add x (opp y)

class Group a => Ring a where
    one :: a
    mul :: a -> a -> a
    die :: a -> a -> (a,a)

class Ring a => Field a where
    inv :: a -> a
    divs :: a -> a -> a
    divs x y = mul x (inv y)

-- EOF
