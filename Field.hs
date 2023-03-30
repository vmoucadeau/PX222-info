
module Field where
import Ring
-- Définition de la classe Corps

class Ring a => Field a where
  inv :: a -> a
