module TracerN where
import Rules
import Utils
import State 
import Data.Maybe (fromJust)
import qualified Data.Map as Map

type Aplication a = (Rule a, Int) 

type Tracer a = (a, ([Aplication a], [[Rule a]]))

type Tracers a = Map.Map a ([Aplication a], [[Rule a]])

type TracerStates a = Map.Map State (Tracers a)

flatten :: TracerStates a -> [(State, [Tracer a])]
flatten trs = [(s, Map.toList tr) | (s, tr) <- Map.toList trs] 

point :: Tracer a -> a
point  = fst 

rulePart :: Tracer a -> ([Aplication a], [[Rule a]])
rulePart = snd

trace :: Tracer a -> [Aplication a]
trace  = fst . snd

buggys :: Tracer a -> [[Rule a]]
buggys = snd . snd 

getTracers :: Ord a => TracerStates a -> Tracers a
getTracers = 
   foldr insertT Map.empty . 
      concatMap snd . flatten 

getFinal :: Ord a => TracerStates a -> Tracers a
getFinal trs = case Map.lookup Accept trs of 
   Nothing -> error "no accepting states"
   Just ts -> ts

insertT :: Ord a => Tracer a -> Tracers a -> Tracers a
insertT t trs = case Map.lookup (point t) trs of 
   Nothing       
      -> Map.insert (point t) (rulePart t) trs 
   Just (tc, bs) 
      -> Map.insert (point t) (tc, bs') trs
         where bs' = nubSub $ bs ++ (buggys t)

lift :: Rule a -> Rule (Tracer a)
lift r = Rule (isBuggy r) (ruleName r) f
   where 
      f (a, (t, b)) = 
         [(a', (ap:t, b')) | (a', ap) <- app] 
            where 
               b' = 
                  if isBuggy r 
                  then map (r:) b 
                  else b 
               app = 
                  [(a'', (r, n)) |
                     (a'', n) <- zip (applyRule r a) [0..]]

placeTracer 
   :: Ord a => (State, Tracer a) -> 
      TracerStates a -> TracerStates a
placeTracer (s, t) trs =
   case Map.lookup s trs of 
      Nothing -> Map.insert s (Map.singleton a rp) trs 
      Just ts -> Map.insert s (insertT t ts) trs
   where
      a  = point t 
      rp = rulePart t

placeTracers 
   :: Ord a => 
      [(State, Tracer a)] -> TracerStates a 
placeTracers ftrs = 
   foldr placeTracer Map.empty ftrs
