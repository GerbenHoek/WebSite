module Calculate where
import FeedBackI
import Utils
import ImportStrat
import qualified Data.Map as Map

calculate :: 
   [String] -> Int -> Ref (Rational) -> Rational -> (String, Int)
calculate bgs n t d = case diag of 
   Nothing -> ("Ik kan de fout niet vinden", 1)
   Just fb -> (feedBack fb, getSub . subTask $ fb)
   where 
      diag = ((flip best) fb' . rank bgs . snd ) <$> 
         (Map.lookup ans tsks)
         where 
            fb' 
               | n == 3 = fb3
               | otherwise = fb0
            ans  
               | n == 3 = [("dy/dx",d)]
               | otherwise = [("inter",d)]
            tsks = applyAllM (fsm $ strat n) t
               where 
                  strat n
                     | n == 3    = trunkStrat 
                     | otherwise = interpolation 


rank :: [String] -> [[Rule a]] -> [String]
rank bgs = head . sorter f . map (map ruleName)
   where
      f = map ((flip index) bgs)


