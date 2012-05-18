module Taches
where

{- Déclarations de type Tache et Graphe
	Comme il est impossible de modifier des variables, nous avons fait le choix de simuler les effets de bords
	en ne créant que des fonctions renvoyant un graphe modifié. Ainsi, appliquer deux modifications successivement
	sur le graphe revient à les composer. On peut remarquer que, par exemple, il est possible "d'annuler" des modifications,
	ce qui revient, en pratique, à remonter le temps (c'est le procédé que nous utilisons pour rédiger le branch and bound).
	Cela se fait très simplement à l'aide de la "monade" State.-}


import Data.Array
import Data.List
import Control.Monad
import Control.Monad.State



data Tache a = Tache {nom :: String,
                    ide :: Integer,
                    cout :: [a],
                    t_in :: a,
                    duree :: a,
                    date_plus_tot :: a,
                    date_plus_tard :: a,
                    marge_totale :: a,
                    marge_libre :: a,
                    successeurs :: [Integer],
                    predecesseurs :: [Integer]
                    } deriving (Eq)
type Graphe a = Array Integer (Tache a)                        

instance (Show a) => Show (Tache a) where
  show (Tache n i c t d dto dta mt ml s p) = "[t=" ++ show t ++ "] " ++ n ++ "\tid=" ++ show i ++ "\tduree=" ++ show d ++ "\t+tot=" ++ show dto ++ "\t+tard=" ++ show dta ++ "\tmtotale="++ show mt ++"\tmlibre=" ++ show ml ++ "\tprec="++show p ++ "\tnxt=" ++ show s
   
--debut nom id duree date_t date_T m_l m_t succ pred = Tache nom id duree date_t date_T m_l m_t succ pred
creerTache nom id duree succ pred = Tache nom id [] (-1::Integer) duree (-1) (-1) (-1) (-1) succ pred
modifTache t g = ((),listArray (bounds g) [if (ide x)==(ide t) then t else x| x <- (elems g)])


{- Action permettant de modifier la tâche t dans le graphe courant -}
modifierTache   :: Tache a -> State (Graphe a) ()
modifierTache t =  State $ modifTache t

{- Accesseurs permettant de modifier les attributs des tâches -}
modifierDatePlusTot (Tache n i c t d1 d2 d3 m1 m2 s p) d = modifierTache $ Tache n i c t d1 d d3 m1 m2 s p 
modifierDatePlusTard (Tache n i c t d1 dt1 dt2 m1 m2 s p) d =  modifierTache $ Tache n i c t d1 dt1 d m1 m2 s p
modifierMargeTotale (Tache n i c t d1 d2 d3 mt ml s p) m = modifierTache $ Tache n i c t d1 d2 d3 m ml s p
modifierMargeLibre (Tache n i c t d1 d2 d3 mt ml s p) m = modifierTache $ Tache n i c t d1 d2 d3 mt m s p
affecterDate (Tache n i c t d1 d2 d3 mt ml s p) d = modifierTache $ Tache n i c d d1 d2 d3 mt ml s p

{-Actions permettant de récuperer les tâches spécifiées dans le graphes courant -}
-- Récupération de la tâche d'indice spécifié
recuperer        :: Integer -> State (Graphe a) (Tache a)
recuperer indice = State $ \gr -> (gr ! indice, gr)

-- Récupération de toutes les tâches
recupererTout :: State (Graphe a) [Tache a]
recupererTout = State $ \gr -> ((elems gr),gr)

-- Récupération des dépendances d'une tâche
dependances   :: Tache a -> State (Graphe a) [Tache a] 
dependances t =  State $ \gr -> ((map ((gr !)) (predecesseurs t)),gr)

-- Récupération des successeurs d'une tâche
suivantes   :: Tache a -> State (Graphe a) [Tache a]
suivantes t =  foldl f (return []) (successeurs t)
    where f acc elem = do x <- acc
                          y <- recuperer elem
                          return (y:x)
                          
mkTache' nom id c duree pred = Tache nom id c (-1::Double) duree (-1) (-1) (-1) (-1) [] pred
mkTache id c duree succ pred = Tache (show id) id c (-1::Integer) duree (-1) (-1) (-1) (-1) succ pred
ajouterNxt nxt (Tache nom id cout date duree ptot ptard mt ml succ pred) = Tache nom id cout date duree ptot ptard mt ml (nxt:succ) pred


genererGraphe [] = return ()
genererGraphe (tache:reste) = do foldl f (return ()) (predecesseurs tache)
                                 genererGraphe reste
      where f acc i = acc >> recuperer i >>= \ta -> modifierTache (ajouterNxt (ide tache) ta)

initGraphe l = snd.runState (genererGraphe l) $ listArray (minimum [ide t | t <- l],maximum [ide t | t <- l]) l

readTache     :: String -> [Tache Double]
readTache str = genererTaches l1 (map read l2) (map read l3) (map read l4) (map read'' l5)
    where (l1:l2:l3:l4:l5:ls) = map (tail.words) $ lines str
          genererTaches [] [] [] [] [] = []
          genererTaches (nom:ns) (i:is) (c:cs) (d:ds) (nxt:nxts) = (mkTache' nom i c d nxt):(genererTaches ns is cs ds nxts)
          read'' s = (read s)::[Integer]
          
                 


                          
