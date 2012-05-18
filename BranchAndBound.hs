module BranchAndBound
where

{- Implémentation d'une méthode d'évaluation et séparation pour obtenir la solution optimale au problème d'ordonnancement sous contraintes de ressources
		-> evaluerAction : détermine une borne inférieure (borne) et une borne supperieure (évaluation) au sous problème formé des choix
											 précédents auquel on ajoute le choix spécifié (execution d'une tâche ou attendre).
		-> verifierEvaluation : Evalue et filtre un ensemble de décisions possibles pour ne conserver que celles qu'il est interessant d'explorer

		-> actionsAExplorer : à partir d'un ordonnancement partiel (formé d'un ensemble de décisions quant à l'ordonnancement), détermine l'ensemble des
													prochains choix à explorer, après avoir élagué l'arbre grâce à la méthode d'évaluation et séparation (Branching)
		-> branchAndBound : à partir d'un ordonnancement partiel et d'une liste des meilleurs ordonnancements trouvés jusqu'à maintenant, on détermine
												l'ensemble des noeuds qu'il est interessant d'explorer (uniquement si on peut espérer y trouver une meilleure solution)
												puis on les explore
		-> explorer : explore un noeud en enregistrant l'état actuel, puis en appliquant le branchAndBound sur le noeud spécifié. -}

import Data.Array
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

import System.Environment

import Taches
import Scheduler
import PERT
import Approximation
-- Evaluation d'une action sur un ordonnancement (de l'ajout d'une tâche)          

evaluerAction action = do bool <- action
                          resultat <- recupererResultat
                          taches <- modifierGraphe recupererTout
                          let tachesAModifier = [ide ta | ta <- taches, not (ide ta `elem` resultat)]
                              resetDateTot tache = modifierGraphe (modifierDatePlusTot tache (-1)) >> return ()
                              resetDateDebut tache = modifierGraphe (affecterDate tache (-1)) >> return ()
                         
                          foldl (\a e -> a >> modifierGraphe (recuperer e) >>= resetDateDebut) (return ()) tachesAModifier
                          foldl (\a e -> a >> modifierGraphe (recuperer e) >>= resetDateTot) (return ()) tachesAModifier 
                    
                     
                          let dernieresTaches = [ide ta | ta <- taches, null (successeurs ta)]
                          foldl (\a e -> a >> modifierGraphe (recuperer e) >>= modifierGraphe. determinerDatePlusTot >> return ()) (return ()) dernieresTaches
                          
                          copie <- State $ \s -> (s,s)
                          let ((ordre,_),resultatEval) = runState (ordonnancer test_regle) copie
                              evaluation = maximum [t_in t + duree t | t <- ordre]
                              borne = maximum [date_plus_tot t | t <- ordre]
                          return (bool, borne, evaluation, resultatEval)

actionsAExplorer :: (Ord a, Num a) => ([Scheduler a],a) -> State (Scheduler a) ([State (Scheduler a) Bool],[Scheduler a], a)
actionsAExplorer (opts,tmax) = do rea <- recupererTachesRealisables
                                  enc <- recupererEnCours
                                  if null rea && null enc then return ([],opts,tmax)
                                    else if null rea then return ([attendre >> return True],opts,tmax)
                                    else do let actionsPossibles = if null enc then [executerTache t | t <- rea]
                                                                     else (attendre >> return True):[executerTache t | t <- rea]
                                                
                                            
                                            foldl verifierEvaluation (return ([],opts,tmax)) actionsPossibles            
                                            
verifierEvaluation precedent action = precedent >> do      (aExplorer, optimaux, t) <- precedent
                                                           copie <- State $ \s -> (s,s) 
                                                           let ((bool, borne, evaluation, resultatEval),_) =runState (evaluerAction action) copie 
                                                           -- On explore si la durée minimale est inférieure à l'optimum trouvé, et qu'elle est différente de l'ordonnancement
                                                           -- par liste
                                                           if borne < evaluation && borne < t && bool then return (action:aExplorer,optimaux,t) 
                                                           -- On conserve l'optimum si on son evaluation est interessante
                                                             else if evaluation < t && bool then return (aExplorer,[resultatEval],evaluation)
                                                                                         -- Sinon, on coupe
                                                             else return (aExplorer,optimaux,t)                                                                                                            
branchAndBound opts tmax = do (aExplorer,nvOpts,tmax') <- actionsAExplorer (opts, tmax)                                           
                              if null aExplorer then return (if null nvOpts then opts else nvOpts,tmax)
                                else do foldl explorer (return (nvOpts,tmax')) aExplorer
                          
explorer precedent action = do (opts,tmax) <- precedent                                           
                               copie <- State $ \s -> (s,s)
                               let ((nvOpts,tmax'), state) = runState (action >> branchAndBound opts tmax) copie
                               return (nvOpts,tmax')


