module Ring where

-- Définition de la classe Ring

class Ring a where
  zero :: a
  one :: a
  add :: a -> a -> a
  mult :: a -> a -> a

-- Les entiers munis de l'addition et de la multiplication forment un anneau
instance Ring Integer where
  zero = 0
  one = 1
  add a b = a + b
  mult a b = a * b

-- Les réels munis de l'addition et de la multiplication forment un anneau
instance Ring Float where
  zero = 0.0
  one = 1.0
  add a b = a + b
  mult a b = a * b