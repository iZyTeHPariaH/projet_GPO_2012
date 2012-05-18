

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
import BranchAndBound


             

{- Exportation d'un problème d'ordonnancement au plus tôt / au plus tard sous la forme
d'un programme linéaire CPlex-}
                     
contraintes g tache = map (f. (g !)) (successeurs tache)
    where f t = "C" ++ show (ide t) ++ " : "
                ++ "t" ++ show (ide t) ++ " - t" ++ show (ide tache) ++ " >= " ++ show (duree tache) 

cplex :: (Num a, Ord a, Show a) => Graphe a -> Tache a -> String
cplex g fin = "Minimize  \n" ++ 
              "\tobj : t" ++ show (ide fin) ++ "\n" ++
              "Subject to  \n" ++ 
               (unlines $  (map (unlines . (contraintes g)) liste) )  ++ 
              "Bounds  \n" ++
              unlines ( (map bounds_vars liste)) ++ 
              "\nEnd\n"
    where bounds_vars t = "0 <= t" ++ show (ide t) ++ " <= +inf"
          liste = elems g    
export fname g fin = do
  writeFile fname (cplex g fin)




{---------------------------------------------------8< Cut there 8< ------------------------------------
Exemples et tests des algorithmes
utilisation :
           ./Graphe test_graphe.txt
-}


main = do args <- getArgs
          foldl (\a e -> a >> evaluerFichier e) (return ()) args

                          
                                 

initialDump rmax = "Utilisation des ressources :\n" ++ concat (map f (zip rmax [1..])) ++  "\tInstant\tDétail\n" 
    where f (ri,i) = "R"++ show i ++ take (truncate ri) (repeat ' ')
-- Affiche autant de * que de ressources utilisées puis (n-*) espaces, n étant le nombre max de ressources
dump :: (RealFrac a) => [Tache a] -> Integer -> [a]->String
dump listeTaches instant rmax = (foldl (\a e -> if null a then e else a ++ "\t|" ++ e) "" $ map f (zip rmax [0..])) ++ "\t|t=" ++ show instant ++ "\t||"++ detail
    where detail = foldl (\a e -> if null a then e else a ++ ", " ++ e) "" [nom t ++ " (" ++ (show.truncate) (100*( (sum (cout t)) / (sum rmax))) ++ "%)" | t <- listeTaches, t_in t <= fromInteger instant, fromInteger instant < t_in t + duree t] 
          f (ri,i) = take (truncate used) (repeat '*') ++ take (truncate $ ri - used) (repeat ' ') 
              where used = sum [ (cout tache) !! i | tache <- listeTaches, fromInteger instant >= t_in tache, fromInteger instant < t_in tache + duree tache]
                    

-- Création des tâches (slides de cours)
  

-- Création d'une regle qui définit un ordre entre deux tâches
-- (regles de priorité entre les tâches lors d'un conflit
-- de ressources)



optimiser' sch = fst $runState (do s <- State $ \s -> (s,s)
                                   let ((ordre,str),sc) = runState (ordonnancer test_regle) s
                                   branchAndBound [sc] (maximum [t_in t + duree t| t <- ordre])) sch



evaluerFichier fname = do x <- readFile fname
                          let l = lines x
                          case (head l) of
                                "optimal" -> optimiZ (tail l)
                                otherwise -> approcher (tail l)
                         

optimiZ l = do  
                let 
                    res =  (head.tail.words) (head l)
                    taches = readTache.unlines $  (tail l)
                    graph = initGraphe taches
              
                    scheduler = makeScheduler (determinerDates graph) (read res)
                    (opt,state) = optimiser' scheduler
                    
                putStrLn.unlines $ map show $ sortBy (\x y -> toOrdering (t_in x) (t_in y) ) ( elems.graphe $(head opt))
approcher l = do 
                  let 
                    res =  (head.tail.words) (head l)
                    taches = readTache.unlines $  (tail l)
                    graph = initGraphe taches
              
                    scheduler = makeScheduler (determinerDates graph) (read res)
                    (ordre,_) = runState (ordonnancer test_regle) scheduler
                    resultLog = map (\i -> dump (fst ordre) i (read res)) [0..truncate $maximum[t_in t + duree t | t <- fst ordre]]
                  putStrLn "Ordonnancement final :"
                  putStrLn $ (unlines.(map show).fst) ordre
                  putStrLn $ unlines ((initialDump (read res)): resultLog)
                  return ()


                                            
