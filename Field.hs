module Field where
import Ring
-- Définition de la classe Corps

class Ring a => Corps a where
  inv :: a -> a

