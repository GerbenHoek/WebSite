module Search where
import ApplyAll
import StrategyBas
import Utils 
import Refinement
import InterRules
import InterStrat
import qualified Data.Set as Set
import qualified Data.Map as Map

modu :: Rational -> Rational -> Bool
modu a b = mod (floor a) (floor b) == 0 

space1 :: Int -> [Ref Rational]
space1 n = take n
   [start [x1,x2,y1,y2,xv]| 
   [x1,x2,y1,y2,xv] <- rmL 5 10 99
   , x1 < x2 - 2
   , modu y1 5 
   , modu x2 5  
   , y1 - y2 /= 0
   , notIn [0] [x1,x2,y1,y2,xv]
   , x1 * y2 /= x2 * y1
   , abs ((y2 - y1)/(x2-x1)) /= 1
   , xv > x2+5]

space2 :: Int -> [Ref Rational]
space2 n = take n
   [start [x1,x1+1,y1,y2,xv]| 
   [xv,x1,y1,y2] <- rmL 4 10 99
   , modu (x1+1) 5
   , abs (y2 - y1) <= 8
   , y2 - y1 /= 0 
   , x1 * y2 /= (x1+1) * y1
   , notIn [0] [x1,y1,xv]
   , xv > x1+6
   ]

space3 :: Int -> [Ref Rational]
space3 n = take n 
   [start [x1,x2,y1,y2,0]| 
   [x1,x2,y1,y2] <- rmL 4 10 70
   , y1 - y2 /= 0
   , modu x1 5 
   , x1 < x2 - 2
   , x1 * y2 /= x2 * y1
   , abs ((y2 - y1)/(x2-x1)) /= 1
   , notIn [0] [x1,x2,y1,y2]
   ]

divers012 :: Ref Rational -> Int 
divers012 = ((-1) *) . (Set.size . Set.fromList . applyAll interpolation)

divers3 :: Ref Rational -> Int 
divers3 = ((-1) *) . Set.size . Set.fromList . applyAll trunkStrat 

par0 = 
   writeFile "par0" . 
   show . map Map.toList . 
   take 50 . sorter divers012 $ 
   space1 1000

par12 = writeFile "par12" . 
   show . map Map.toList . 
   take 50 . sorter divers012 $ 
   space2 1000

par3 = writeFile "par3" . 
   show . map Map.toList . 
   take 50 . sorter divers3 $ 
   space3 1000