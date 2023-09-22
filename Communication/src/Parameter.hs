module Parameter where
import qualified Data.Map as Map
import System.Random
import Data.Time.Clock.System

time :: IO Int
time = fromIntegral . systemSeconds <$> getSystemTime

trueRand :: Int -> Int -> IO Int
trueRand k1 k2 = rms k1 k2 <$> time
   where 
      rms k1 k2 = 
         fst . randomR (k1,k2) . mkStdGen 

randEl :: [a] -> IO a
randEl as = case as of 
   []  -> error "randEl: empty list"
   as' -> (as!!) <$> trueRand 0 (-1 + length as) 

loadTasks :: IO [[(String, Rational)]]
loadTasks = mapM loadTask' [0..3]
   where
      loadTask' n = do
         t'' <- readFile file
         let t' = read t'' :: [[(String, Rational)]]
         t <- randEl t'
         return t
            where 
               file 
                  | n == 0 = "par0"
                  | n == 1 = "par12"
                  | n == 2 = "par12"
                  | n == 3 = "par3"




