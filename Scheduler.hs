module Scheduler
where

{- Définition de l'ordonnanceur
	C'est un ensemble contenant : -> un compteur determinant l'instant où nous sommes
																-> une liste contenant la liste des tâches en cours d'execution
																-> une liste contenant la liste des tâches que nous pouvons réaliser
																-> la liste des ressources disponibles
																-> la liste des tâches déjà ordonnancées (champs utilisé pour le branch and bound)
	Nous l'encapsulons également dans une monade State, pour plus de confort d'écriture. -}


import Control.Monad
import Control.Monad.State
import Data.Array
import Taches


                 

data Scheduler a = Scheduler {time :: a,
                              graphe :: Graphe a,
                              realisables :: [Tache a],
                              en_cours :: [Tache a],
                              ressources :: [a],
                              result :: [Integer]}
makeScheduler g ress = Scheduler 0 g [t | t <- (elems g), null (predecesseurs t)] [] ress []


{- Encapsulation d'un ordonnanceur dans une monade State.
Ainsi, il suffit de binder (>>=) les actions modifiant les attributs d'un ordonnanceur entre elles. 
En fait, il s'agit de voir un Ordonnanceur comme un état. Les calculs à états sont en fait des fonctions
qui, à un état donné, associent une valeur et le nouvel état.-}
recupererInstant = State $ \ s@(Scheduler t g r e res ret) -> (t,s)
recupererTachesRealisables = State $ \ schedule -> (realisables schedule, schedule)
recupererEnCours = State $ \schedule -> (en_cours schedule, schedule)
recupererRessources = State $ \schedule -> (ressources schedule, schedule)
recupererResultat = State $ \schedule -> (result schedule, schedule)

modifierTachesRealisables nouvelleListe = State $ \(Scheduler t g r e res ret) -> ((),Scheduler t g nouvelleListe e res ret) 
modifierEnCours nouvelleListe = State $ \(Scheduler time gr rea en res ret) -> ((),Scheduler time gr rea nouvelleListe res ret)
modifierRessources nouvelleListe = State $ \(Scheduler time gr rea en res ret) -> ((),Scheduler time gr rea en nouvelleListe ret)
modifierResultat nouvelleListe = State $ \(Scheduler time gr rea en res ret)-> ((), Scheduler time gr rea en res nouvelleListe)
modifierInstant t = State $ \(Scheduler time gr rea e res ret) -> ((),Scheduler t gr rea e res ret)
copier getter accesseur tache = do val <- getter
                                   accesseur (tache:val)
supprimer getter accesseur tache = do val <- getter
                                      accesseur [t | t <- val, (ide t) /= (ide tache)]


ajouterAuResultat tache = do x <- recupererResultat
                             modifierResultat ((ide tache):x)
