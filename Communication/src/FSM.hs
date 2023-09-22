module FSM where
import Data.Maybe (fromJust)
import State
import Rules
import Trails
import TracerN
import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set

type FSM a = Map.Map State [(Symbol a, State)]

data Symbol a = Symb a | Eps 
   deriving (Eq, Ord, Show)

fmapM :: (a -> b) -> FSM a -> FSM b
fmapM g fm = Map.map (map g') fm 
   where 
      g' (Symb a, s) = (Symb (g a), s)
      g' (Eps, s)    = (Eps, s)

remainders :: Ord a => [Trail a] -> [Trail a]
remainders = getFix f   
   where 
      f ss = setNub $ 
         [s' | s <- ss, (r,s') <- firsts s] ++ ss

fsm :: Ord a => Trail a -> FSM a
fsm s = 
   Map.fromList $
   (zip (map getSt rmds) $
      [[(Symb r, getSt s'')| (r, s'') <- firsts s'] 
         ++ if empty s' 
            then case s' 
                   of Many t -> [(Eps, Accept)] 
                      _      -> [(Eps, getSt s'') | (r',s'') <- firsts s']
            else []
               | s' <- rmds]) ++ [(Accept, [])] 
   where 
      rmds = remainders [s]
      getSt s' = fromJust . lookup s' $ states 
         where 
            states = 
               (s, Start) : (Succeed, Accept) : sts
                  where
                     sts = zip sts' (map S [1..])
                     sts' = setDifference 
                               rmds [s, Succeed]

next ::
   Ord a => 
   FSM (Rule a) -> State -> 
   Tracer a -> [(State, Tracer a)]
next ms s1 t = 
   [(s2, t') | (Symb r, s2) <- steps, 
                         t' <- applyRule (lift r) t]
      ++ [(s2, t) | (Eps, s2) <- steps]
         ++ case steps of [] -> [(Accept, t)] 
                          _  -> []
   where
      steps = fromJust . Map.lookup s1 $ ms

nexts :: 
   Ord a =>
   FSM (Rule a) -> 
   TracerStates a -> [(State, Tracer a)]
nexts ms trs =
   concat 
      [concatMap (next ms s) t | 
         (s, t) <- (flatten trs)]

applyM 
   :: Ord a => 
      FSM (Rule a) -> 
      TracerStates a -> TracerStates a
applyM fm = placeTracers . nexts fm

start :: a -> TracerStates a
start a = 
   Map.singleton Start 
      (Map.singleton a ([], [[]])) 

applyAllM
   :: 
      Ord a => 
      FSM (Rule a) -> 
      a -> Tracers a
applyAllM ms =
   fromJust .
   Map.lookup Accept .
   iterWhileP1 (not.done) (applyM ms) . 
   start
   where 
      done = 
         ([Accept] ==) . 
         Map.keys . 
         Map.filter (not.null) 

applyN 
   :: 
      Ord a => 
      Int -> 
      FSM (Rule a) -> 
      a -> Tracers a
applyN n ms = 
   getTracers . 
   last .
   take n . 
   iterate (applyM ms) .
   start 



