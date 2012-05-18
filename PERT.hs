module PERT
where


{- Implémentation de la méthode PERT permettant d'optimiser l'ordonnancement d'un projet sans
   contraintes de ressources.
   -> determinerDatePlusTot			: Détermination de la date de début au plus tôt de la tâche spécifiée, et de ses prédécesseurs récursivement.
   -> determinerDatePlusTard		: Détermination de la date au plus tard de la tâche spécifiée, ainsi que de ses successeurs récursivement
   -> determinerMargesLibres		: Détermination de la marge libre d'une tâche (datePlusTard - datePlusTot)
   -> determinerMargesTotales   : Détermination de la marge totale d'une tâche-}

import Data.Array
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import Taches


{-Détermination de la marge totale :
 Nous calculons l'attribut marge_totale de chaque tâche du graphe -}

determinerMargesTotales :: (Num a) => State (Graphe a) ()
determinerMargesTotales = do taches <- recupererTout
                             foldl (\a e -> a >> modifierMargeTotale e (date_plus_tard e - date_plus_tot e)) (return ()) taches
{- Détermination de la marge libre :
 Si nous n'avons pas de successeurs, c'est égal à la marge totale, sinon, nous déterminons la durée minimum que
 nous pouvons nous permetttre de perdre sans retarder les tâches suivantes. -}

determinerMargeLibre t = if (null . successeurs) $ t then modifierMargeLibre t (marge_totale t)
                         else if (marge_libre t >= 0) then return ()
                         else do
                                nxt <- suivantes t
                                let m = minimum [date_plus_tot x - date_plus_tot t - duree t | x <- nxt]
                                modifierMargeLibre t m
                                
determinerMargesLibres :: (Ord a, Num a) => State (Graphe a) ()
determinerMargesLibres = do taches <- recupererTout                 
                            foldl (\a e -> a >> determinerMargeLibre e) (return ()) taches


determinerDatePlusTot t =
                          if t_in t >= 0 then modifierDatePlusTot t ( t_in t) >> return ( t_in t)
                          else if (null . predecesseurs) $ t then modifierDatePlusTot t 0 >> return 0
                          else if (date_plus_tot t) >= 0 then return (date_plus_tot t)
                          else do
                                 dep <- dependances t
                                 foldl (\x y -> x >> determinerDatePlusTot y) (return 0) dep
                                 new_dep <- dependances t
                                 let d = maximum [duree x + date_plus_tot x | x <- new_dep]
                                 modifierDatePlusTot t d
                                 return d

               

determinerDatePlusTard t = if (null . successeurs) t then modifierDatePlusTard t (date_plus_tot t) >> return (date_plus_tard t)
                           else if (date_plus_tard t) >= 0 then return (date_plus_tard t)
                           else do
                                  nxt <- suivantes t
                                  foldl (\ x y -> x >> determinerDatePlusTard y) (return 0) nxt
                                  new_nxt <- suivantes t
                                  let d = minimum [date_plus_tard x - duree t | x <- new_nxt]
                                  modifierDatePlusTard t d
                                  return d

determinerDates g = snd $ runState acc g
    where (i,j) = bounds g
          acc = do x <- recuperer j
                   determinerDatePlusTot x
                   y <- recuperer i
                   determinerDatePlusTard y
                   determinerMargesTotales
                   determinerMargesLibres



