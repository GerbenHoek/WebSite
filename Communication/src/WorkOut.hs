module WorkOut where
import Refinement
import Utils
import Data.Maybe

wkSelect :: Int -> Ref Rational -> String
wkSelect n 
   | n==1 = wk12 
   | n==2 = wk12 
   | n==3 = wk3 

wk12 :: Ref Rational -> String
wk12 rf = 
   "In 1 stap van x verandert\n" ++
   "y met " ++ show (y2-y1) ++ ", vanaf " ++ show x2 ++ " is het nog\n" ++
   show (xv - x2) ++ " stappen naar " ++ show xv ++ " dus: \n\n" ++
   "? = " ++ show y2 ++ " + " ++ show (xv-x2) ++ " * " ++ show (y2-y1)
   ++ " = " ++ show (y2 + (xv-x2) * (y2-y1))
   where
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf) 

wk3 :: Ref Rational -> String
wk3 rf = 
    "In " ++ show (x2-x1) ++" stappen van x verandert y met " 
     ++ show (y2-y1) ++ ",\n" ++
    "per stap van x is dat dus: " 
     ++ show (y2-y1) ++ "/" ++ show (x2-x1) ++ " = " ++ show ans 
     where 
        ans = 
           fromRational 
           (
           rounder 3 $
           toRational (y2-y1) / toRational (x2-x1)
           ) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf) 
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf) 