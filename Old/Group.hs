module Group where

-- Définition de la classe Group

class Group a where
  unit :: a
  inverse :: a -> a
  operation :: a -> a -> a

-- Les réels (non nuls) munis du produit forment un groupe
instance Group Float where
  unit = 1.0
  inverse x = 1/x
  operation a b = a * b

-- Définition de la classe Anneau

class Anneau a where
  zero :: a
  addition :: a -> a -> a
  oppose :: a -> a
  un :: a
  produit :: a -> a -> a

-- Les entiers munis de l'addition forment un anneau
instance Anneau Integer where
  zero = 0
  addition x y = x+y
  oppose x = -x
  un = 1
  produit x y = x*y
