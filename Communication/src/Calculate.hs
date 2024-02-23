module Calculate where
import FeedBackI
import FeedBack0
import FeedBack1
import FeedBack2
import FeedBack3
import FeedBack4
import FeedBack10
import FeedBack11
import FeedBack12
import FeedBack13
import FeedBack14
import Utils
import ImportStrat
import Data.Maybe
import qualified Data.Map as Map

calculate :: 
   [String] -> Int -> [(String, Rational)] -> Rational -> (String, Int)
calculate bgs n t d = (feedBack fb, getSub . subTask $ fb)
   where 
      fb = ((flip best) (fb' t')) . 
         fromMaybe [""] $
         (rank bgs) <$> (diagnose (strat n) t' ans)
         where 
            t' = Map.fromList t
            fb' 
               | n == 4 = fb4 
               | n == 3 = fb3
               | n == 2 = fb2
               | n == 1 = fb1
               | n == 0 = fb0
               | n == 14 = fb14
               | n == 13 = fb13
               | n == 12 = fb12
               | n == 11 = fb11
               | n == 10 = fb10
            ans  
               | n == 3  = Map.singleton ("dy/dx") d
               | n == 13 = Map.singleton ("g") d
               | otherwise = Map.singleton ("inter") d
            strat n
               | n == 3    = trunkStrat 
               | n == 13   = trunkStratE 
               | n <  10   = interpolation
               | otherwise = exponS

rank :: [String] -> [[Rule a]] -> [String]
rank bgs = head . sorter f . map (map ruleName)
   where
      --f = map ((flip index) bgs) 
      f rs = (length rs, map ((flip index) bgs) rs) 