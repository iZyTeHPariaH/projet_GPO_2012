module Approximation
where

{- Implémentation de l'algorithme de listes pour déterminer une solution réalisable au problème d'ordonnancement sous contraintes de ressources limitées.
		-> realisable_p : détermine si une tâche est réalisable (si toutes ses dépendances sont satisfaites)
		-> executerTache : ordonnance une tâche (affecte une date de début et diminue les ressources)
		-> attendre : avance le compteur de temps jusqu'à la date de fin de la prochaine tâche en cours
		-> selectionnerTaches : choisit une ou plusieurs tâches à executer en fonction de l'heuristique choisie
		-> ordonnancer : détermine une date de début pour chaque tâche selon l'heuristique choisie
-}

import Data.Array
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import System.Environment

import Taches
import Scheduler
import PERT


toOrdering x y 
 | x < y     = LT
 | x == y    = EQ
 | otherwise = GT
      

executerAction action (Scheduler time gra rea enc res ret) = (val, Scheduler time gr rea enc res ret)
  where (val,gr) = (runState action) gra        
modifierGraphe action = State $ executerAction action        


realisable_p t = do prec <- modifierGraphe (dependances t)
                    tic <- recupererInstant
                    return $ and (map (\tache -> (t_in tache >= 0) && (tic >= (t_in tache + duree tache))) prec)
                                  
{- Execute une tâche dans l'ordonnanceur si les ressources sont suffisantes.-}
executerTache t = do instant <- recupererInstant
                     realisable <- recupererTachesRealisables
                     ressourcesDispo <- recupererRessources
                     if not (and $ map (0<=) (zipWith (-) ressourcesDispo (cout t))) then return False
                       else do enCours <- recupererEnCours
                                       
                            -- On affecte une date d'execution à la tâche et on n'oublie pas de récuperer sa version modifiée.           
                               modifierGraphe (affecterDate t instant)
                               ta <- modifierGraphe (recuperer (ide t))
                               ajouterAuResultat ta
                            --On déplace la tâche de la liste des tâches réalisables à la liste des tâches en cours
                            --et on alloue les ressources
                               modifierTachesRealisables [taches | taches <- realisable, (ide taches) /= (ide t)]
                               modifierEnCours (ta:enCours)
                               modifierRessources (zipWith (-) ressourcesDispo (cout ta))
                               return True
{- Action permettant de terminer une tâche.-}                               
terminerTache t = do realisable <- recupererTachesRealisables
                     ressourcesDispo <- recupererRessources
                     enCours <- recupererEnCours
                     
                     -- On supprime la tâche des tâches réalisables, on libère les ressources.
                     -- On récupère la liste des tâches suivantes, et on vérifie si leur dépendances sont satisfaites
                     -- auquel cas nous les ajoutons à la la liste des tâches réalisables.
                     nxt <- modifierGraphe (suivantes t)                     
                     nxt_realisable <- foldl (\a e ->a >>= (\xs -> do b <- realisable_p e
                                                                      rea <- recupererTachesRealisables
                                                                      if b && (not ((ide e) `elem` (map ide rea))) then return (e:xs) else return xs)) 
                                                     (return []) nxt
                     
                     modifierEnCours [taches | taches <- enCours, (ide taches) /= (ide t)]
                     modifierRessources (zipWith (+) ressourcesDispo (cout t))
                     modifierTachesRealisables (realisable ++ nxt_realisable)
                                          
{- Action permettant de simuler l'attente -}
attendre :: (Num a,Ord a) => State (Scheduler a) ()
attendre = do instant <- recupererInstant
              enCours <- recupererEnCours
              
              -- On calcule la plus petite date de fin d'execution à partir des tâches en cours de traitement.
              -- Nous avançons l'horloge jusqu'à cette date, puis nous terminons toutes les tâches nécessaires.
              let dateMin =  minimum [duree tache + t_in tache | tache <- enCours]
                  tachesATerminer = [tache | tache <- enCours, (duree tache + t_in tache) == dateMin]
              modifierInstant dateMin
              foldl (\a e -> a >> terminerTache e) (return ()) tachesATerminer
              

{- Selection des tâches à executer parmis la liste des tâches réalisables (selon une regle de priorité en
cas de conflit de ressources -}
selectionnerTaches regle = do tachesRealisables <- recupererTachesRealisables
                              foldl (\a e -> a >> executerTache e) (return True) (sortBy regle tachesRealisables)

{- Action sur un ordonnanceur permettant d'ordonner la liste des tâches associées -}
ordonnancer regle = ordonnancer' regle []
ordonnancer' regle str= do rea <- recupererTachesRealisables
                           enc <- recupererEnCours
                           if null rea && null enc
                             then  do ret <- modifierGraphe recupererTout
                                      let sortfun t1 t2 = case (t_in t1) <= (t_in t2) of
                                                          True -> LT
                                                          False -> GT
                                      return $ (sortBy sortfun ret, str)
                                  
                             else  do selectionnerTaches regle
                                      attendre
                                      logs <- logz
                                      ordonnancer' regle (str ++ logs)
logz :: (Show a) => State (Scheduler a) String
logz = do enc <- recupererEnCours
          rea <- recupererTachesRealisables
          t <- recupererInstant
          return $ "[t=" ++ show t ++ "] exec=" ++ show (map nom enc) ++ ", realisable="++show (map nom rea)++"\n"

test_regle t1 t2 = case x of
  EQ ->  case y of
    EQ -> z
    otherwise -> y
  otherwise -> x
  where x = toOrdering (date_plus_tard t1) (date_plus_tard t2)
        y = toOrdering (date_plus_tard t1 + duree t1) (date_plus_tard t2 + duree t2)
        z = toOrdering (duree t1) (duree t2)

test_regle' t1 t2 = toOrdering (date_plus_tard t1) (date_plus_tard t2)
