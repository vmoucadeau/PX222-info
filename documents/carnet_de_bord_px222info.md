
Groupe : Alexandre VINHAS et Vincent MOUCADEAU

# Séance 1 - 20/03/2023

Lors de cette séance, nous avons essayé de nous approprier au mieux le sujet, à savoir le chiffrement AES. Nous avons commencé par essayer de comprendre les parties 3 et 4 de la documentation FIPS, à savoir les notations/conventions et les préliminaires mathématiques. Nous avons commencé à réfléchir à une implémentation de l'addition des polynômes sur GF(256) en Haskell, en faisant quelques exemples.
Pour la prochaine séance, nous avons prévu de :
- Terminer l'implémentation de l'addition des polynômes sur GF(256) en Haskell (Vincent)
- Commencer/Terminer l'implémentation de la multiplication (et la multiplication par x) en Haskell (Alexandre)
- Commencer à lire la suite du standard FIPS (spécifications de l'algorithme) (Vincent et Alexandre)

Pour la séance du 31/03/2023, nous avons prévu de finir l'implémentation de la multiplication modulo un polynôme, ainsi que l'inverse d'un polynôme

# Séance 2 - 31/03/2023

Lors de cette séance, nous avons essayé de rassembler le plus clairement possible les structures algébriques que nous allons utiliser par la suite dans le projet. Nous avons également amélioré le "newtype" Polynome avec un type générique a pour pouvoir l'instancier dans l'anneau Z, GF256...
Nous allons bientôt terminer la division euclidienne de polynômes ainsi que l'algorithme d'euclide étendu qui nous permettra de trouver l'inverse d'un polynôme dans GF256.
Pour la prochaine séance, nous avons prévu de :
- Finir le corps GF256
- Commencer à implémenter l'algorithme d'AES

# Ven 31/03/2023 à 17h52 
- Ajout du modulo de 2 polynômes
- Ajout de la division de 2 polynômes
- Euclide étendu commencé

# Ven 31/03/2023 à 19h02
- Euclide étendu terminé (tests à faire) (quelques trucs à corriger, boucle infinie)

# Sam 01/04/2023 à 11h50
- Quelques corrections sur divpol, modpol et euclidepol
- divpol, modpol et euclidepol prennent en charge les polynômes de degré 0
- Tests approfondis à faire

# Sam 01/04/2023 à 18h
- Quelques corrections sur poly.hs
- Amélioration de Struct.hs et Scalaire.hs (added Show, Eq)
- Création GF.hs, implémentation de toutes les opérations de corps

# Jeu 27 avril à 11h06.
- Partie préliminaires Mathématiques terminés

# Séance 3 - 28/04/2023
Lors de cette séance, nous avons pu commencer l'implémentation de l'algorithme d'AES (partie 5 : spécifications de l'algorithme). Nous avons commencé à coder les transformations individuelles (SubBytes et ShiftRows). Celles-ci sont presques terminées.
Pour la prochaine séance, nous avons prévu de :
- Terminer les transformations
- Commencer la partie 5.2 (Key Expension)
- Commencer si possible la partie 5.3 (Inverse Cipher)