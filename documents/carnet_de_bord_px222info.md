
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


# Lundi 08/05/2023 à 00h34
- Ajout de shiftrows et mixcolumns dans la branche master
- Tests à faire pour mixcolumns (shiftrows semble ok)

# Mardi 09/05/2023
- Ajout des polynômes de types génériques
- Modification des structures pour utiliser les polynômes (State définis comme des polynômes de GF4X (mots de 32 bits) et GF4X comme des polynômes de GF256) et GF256 comme des polynômes de Z/2Z
- Fonctions de parsing pour faciliter les tests
- Tests des fonctions de parsing
- Ajout des fonctions de transformations des states subbytes, shiftrows (versions simplifiées grâce aux polynômes), mixcolumns
- Début de addroundkey
- Lecture de la section 5.2 (Key Expension)

# Séance 4 - 10/05/2023
Lors de cette séance, nous avons terminé l'implémentation des transformations sur les states (subbytes, shiftrows, mixcolumns et addroundkey). Nous avons également presque terminé la partie Key Expansion (petit bug à corriger). Enfin, nous avons commencé l'implémentation en C afin de réfléchir aux types de données que nous allons utiliser.
Pour la prochaine séance, nous avons prévu de :
- Terminer la partie Cipher
- Commencer/Terminer la partie Inverse Cipher
- Continuer l'implémentation en C

# Séance 5 - 16/05/2023
Travail réalisé avant la séance :
- Partie Haskell terminée (programme capable de chiffrer/déchiffrer un bloc avec une clé de 128, 192 ou 256 bits)
- Partie C commencée (opérations de base sur les polynômes, définition des types...)
Lors de cette séance, nous avons apporté quelques modifications au programme Haskell pour le rendre plus facile à utiliser (affichage du résultat sous forme de String, Parsers plus flexibles...). Nous avons également ajouté la multiplication entre deux gf256 en utilisant la méthode de la multiplication par x (section 4.2.1). Nous n'implémenterons pas la division euclidienne des polynômes ainsi que l'algorithme d'euclide en C.

Pour la prochaine séance, nous avons prévu de : 
- Avancer/Terminer le programme en C
- Rédiger un document détaillant le fonctionnement des deux programmes

# Mercredi 17 mai - 17h27
- Améliorations sur le programme Haskell (possibilité de chiffrer/déchiffrer un texte entier)
- Ajout de fonctions sur les mots de 32 bits (addition, multiplication)

# Samedi 20 mai - 23h53
- Implémentation en C terminée !
- Programme capable de chiffrer/déchiffrer du texte avec une clé de 128 bits (à tester avec 192 et 256 bits)
- Reste à faire : optimiser un peu le code, le rendre pratique à utiliser, chiffrer/déchiffrer des fichiers, faire une documentation...

# Lundi 29 mai - 11h33
- Jalon intermédiaire terminé (dans documents/jalon/jalon.pdf)
- Fonctions encodetext et decodetext améliorées en C (prend en arguments des textes de longueur variable)

# Lundi 12 Juin
- Réorganisation des fichiers du projet en C (création du dossier plugins)
- Plugin pour le chiffrement/déchiffrement de fichiers en ECB
- Premier test de chiffrement sur un fichier de 300 ko : 1,189s (252,3ko/s)

# Mardi 13 Juin
- Plugin pour l'implémentation de l'API demandée dans l'extension du sujet
- Plugin pour le chiffrement/déchiffrement de fichiers bitmap en conservant l'en-tête
- Modification des fonctions encode_block et decode_block pour prendre en charge le CBC
- Modification des plugins api, files et bitmap pour prendre en charge le CBC
- On remarque une différence visuelle notable entre un fichier bmp chiffré en mode ECB et CBC
- Ecriture des tests unitaires pour certaines fonctions du programme
- Correction des fuites de mémoire à l'aide des options -fsanitize

# Mercredi 14 Juin
- Ecriture de la fonction file_entropy pour mesurer l'entropie d'un fichier
- On a mesuré l'entropie de plusieurs fichiers : bitmap_original (3,63), bitmap_ciphered_ecb128 (6,59) et bitmap_ciphered_cbc128 (7,99)
- Début de la réécriture du programme en un seul fichier avec la nouvelle structure

# Jeudi 15 Juin
- Optimisation du programme C actuel (table de multiplications, améliorations mineures)
- Performances actuelle (ECB) :

|Version|Taille du fichier|Temps de chiffrement|Temps de déchiffrement|Vitesse de chiffrement|Vitesse de déchiffrement|
|:-:    |:-:    |:-:    |:-:    |:-:    |:-:    |
|V1|300 ko|1,189s|1,189s|252,3 ko/s|252,3 ko/s
|V2 (gcc)|1,1 Go|2 min 6s|       |8,62 mo/s|
|V2 (gcc Ofast)|1,1 Go|2 min 6s|       |8,62 mo/s|
|V2 (gcc O3 flto march=native)|1,1 Go|2 min 1s|1 min 47s|9 mo/s|10,28 mo/s
|V2 (clang Ofast)|1,1 Go|1 min 47s|2 min 3s|10,12 mo/s|8,8 mo/s|
|V2 (clang O3 flto march=native)|1,1 Go|1 min 29s|2 min 3s|12,35 mo/s|
|V3 (clang O3 flto march=native)|1,1 Go|13,036s|11,469s|84,38 mo/s|95,9 mo/s