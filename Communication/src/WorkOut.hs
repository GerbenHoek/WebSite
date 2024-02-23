module WorkOut where
import Refinement
import Utils
import Data.Maybe
import ExpRules
import qualified Data.Map as Map

wkSelect :: Int -> [(String, Rational)] -> String
wkSelect n 
   | n==1  = wk1 
   | n==2  = wk2 
   | n==3  = wk3 
   | n==4  = wk4
   | n==11 = wk11
   | n==12 = wk12
   | n==13 = wk13
   | n==14 = wk14
   | otherwise = wk1

wk1 :: [(String, Rational)] -> String
wk1 rf' = "In dit speciale geval verandert x slechts met " ++
   show x2 ++ " - " ++ show x1 ++ " = 1 stap." ++
   " Dus in 1 stap van x verandert y met:<br>" 
   ++ show y2 ++ " - " ++ show y1 ++ " = "++ show (y2-y1) ++ 
   "<br>Vanaf " ++ show x2 ++ " is het nog " 
   ++ show xv ++ " - " ++ show x2 ++ " = " ++ show (xv - x2) ++ 
   " stappen naar " ++ show xv ++ " dus: <br><br>" ++
   "? = " ++ show y2 ++ " + " ++ show (xv-x2) ++ " &#183; " ++ show (y2-y1)
   ++ " = " ++ show (y2 + (xv-x2) * (y2-y1))
   where
      rf = Map.fromList rf'
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf) 

wk2 :: [(String, Rational)] -> String
wk2 rf' = 
   "In 1 stap van x verandert " ++
   "y met " ++ show (y2-y1) ++ " (dat is gegegeven)" ++ 
   ", vanaf " ++ show x2 ++ " is het nog " 
   ++ show xv ++ " - " ++ show x2 ++ " = " ++ show (xv - x2) ++ 
   " stappen naar " ++ show xv ++ " dus: <br><br>" ++
   "? = " ++ show y2 ++ " + " ++ show (xv-x2) ++ " &#183; " ++ show (y2-y1)
   ++ " = " ++ show (y2 + (xv-x2) * (y2-y1))
   where
      rf = Map.fromList rf'
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf) 

wk3 :: [(String, Rational)] -> String
wk3 rf' = 
    "In " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ show (x2-x1) ++
    " stappen van x verandert y <br> met " 
     ++ show y2 ++ " - " ++ show y1 ++ " = " ++ show (y2-y1) ++ "<br>" ++
    "per stap van x is dat dus: " 
     ++ show (y2-y1) ++ "/" ++ show (x2-x1) ++ " = " ++ show ans 
     where 
        rf = Map.fromList rf'
        ans = 
           fromRational 
           (rounder 2 $ toRational (y2-y1) / toRational (x2-x1)) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf) 
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf) 

dots :: Rational -> [Char]
dots a = 
   if trunc a /= a 
   then show a' ++ "&#183;&#183;&#183;"
   else show a' 
   where 
      trunc x = 
         (signum x) * (fromIntegral $ floor $ abs $ 10^3 * x)/10^3
      a' = (fromRational $ trunc a) :: Double 

sup :: String -> String
sup s = "<sup>" ++ s ++ "</sup>"

sub :: String -> String
sub s = "<sub>" ++ s ++ "</sub>"

wk4 :: [(String, Rational)] -> String
wk4 rf' = 
   "In " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ show (x2-x1) ++
   " stappen van x verandert y <br> met " ++ 
   show y2 ++ " - " ++ show y1 ++ " = " ++ show (y2-y1) ++ "<br>" ++
   "per stap van x is dat dus: " ++ show (y2-y1) ++ "/" ++ show (x2-x1) ++ 
   " ( = " ++ dots dyDdx ++ ")<br><br>" ++  
   
   "Vanaf " ++ show x2 ++ " is het nog " 
   ++ show xv ++ " - " ++ show x2 ++ " = " ++ show (xv - x2) ++ 
   " stappen naar " ++ show xv ++ " dus: <br><br>" ++
   "? = " ++ show y2 ++ " + " ++ show (xv-x2) ++ " &#183; " ++ 
   show (y2-y1) ++ "/" ++ show (x2-x1) ++
   " = " ++ show ans
     where 
        rf = Map.fromList rf'
        ans = 
           fromRational 
              (rounder 2 $ 
              (toRational y2) + (toRational (xv-x2)) * dyDdx) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf)
        xv = round (fromJust $ "xv" << rf)  
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf)
        dyDdx = toRational (y2-y1) / toRational (x2-x1)

wk11 :: [(String, Rational)] -> String
wk11 rf' =
   "In dit bijzondere geval wordt de waarde van y wordt met een factor " ++ 
   show y2 ++ "/" ++ show y1 ++ " (=" ++ dots fac ++ ")" ++
   " vermenigvuldigd in slechts " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ show (x2-x1) ++
   " stap van x.<br>" ++
   "Daarom is de groeifactor nu gelijk aan: " ++ dots fac ++ "<br>" ++
   "De gevraagde waarde kunnen we berekenen door met de onafgeronde groeifactor " ++ 
   show (xv-x2) ++ " stappen verder te rekenen vanaf " ++ show y2 ++ ":<br>" ++
   "? = " ++ show y2 ++ " &#183; " ++ dots fac ++ sup (show (xv-x2)) ++ " = " ++
   show ans 
    where 
        rf = Map.fromList rf'
        fac = (toRational y2)/(toRational y1)
        ans = 
           fromRational 
              (rounder 3 $ 
              (toRational y2) * (fac ^^ (xv-x2))) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf)
        xv = round (fromJust $ "xv" << rf)  
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf)

wk12 :: [(String, Rational)] -> String
wk12 rf' =
   "Er is gegegeven dat de groeifactor per stap van x gelijk is aan: " ++ show grow ++
   ". De gevraagde waarde kunnen we berekenen door met de groeifactor " ++ 
   show xv ++ " - " ++ show x2 ++ " = " ++ show (xv-x2) ++ 
   " stappen verder te rekenen vanaf " ++ show y2 ++ 
   ". In iedere stap wordt er vermenigvuldigd met de groeifactor dus:<br><br>" ++
   "? = " ++ show y2 ++ " &#183; " ++ dots g ++ sup (show (xv-x2)) ++ " = " ++
   show ans 
    where 
        rf = Map.fromList rf'
        fac = (toRational y2)/(toRational y1)
        grow = fromRational (rounder 3 g) :: Double
        ans = 
           fromRational 
              (rounder 3 $ 
              (toRational y2) * (g ^^ (xv-x2))) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf)
        xv = round (fromJust $ "xv" << rf)  
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf)
        g  = rounder 3 ((toRational y2)/(toRational y1)) ^. (1/toRational(x2-x1))
        a ^. b = fromMaybe 0 $ power a b

wk13 :: [(String, Rational)] -> String
wk13 rf' =
   "De waarde van y wordt met een factor " ++ show y2 ++ "/" ++ show y1 ++
   " (=" ++ dots fac ++ ")" ++
   " vermenigvuldigd in " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ show (x2-x1) ++
   " stappen van x.<br>Dit is dan de groeifactor per " ++ show (x2-x1) ++
   " stappen:<br>g" ++ sub (show (x2-x1) ++" stappen") ++ " = " ++ show y2 ++ "/" ++ show y1 ++
   "<br>Deze groeifactor kunnen we omrekenen naar de groeifactor per 1 stap:<br>" ++
   "g" ++ sub "1 stap" ++ " = (" ++ show y2 ++ "/" ++ show y1 ++ ")" ++ 
   sup ("1/" ++ show (x2-x1)) ++ " = " ++ show ans 

    where 
        rf = Map.fromList rf'
        fac = (toRational y2)/(toRational y1)
        ans = fromRational (rounder 3 $ g) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf)
        xv = round (fromJust $ "xv" << rf)  
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf)
        g  =  ((toRational y2)/(toRational y1)) ^. (1/toRational(x2-x1))
        a ^. b = fromMaybe 0 $ power a b

wk14 :: [(String, Rational)] -> String
wk14 rf' =
   "De waarde van y wordt met een factor " ++ show y2 ++ "/" ++ show y1 ++
   " (=" ++ dots fac ++ ")" ++
   " vermenigvuldigd in " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ show (x2-x1) ++
   " stappen van x.<br>" ++
   "Per stap van x is de groeifactor dan:<br>(" ++ show y2 ++ "/" ++ show y1
   ++ ")" ++ sup ("1/" ++ show (x2-x1)) ++ " = " ++ dots g ++ "<br>" ++
   " De gevraagde waarde kunnen we berekenen door met de onafgeronde groeifactor " ++ 
   show (xv-x2) ++ " stappen verder te rekenen vanaf " ++ show y2 ++ ":<br>" ++
   "? = " ++ show y2 ++ " &#183; " ++ dots g ++ sup (show (xv-x2)) ++ " = " ++
   show ans 
    where 
        rf = Map.fromList rf'
        fac = (toRational y2)/(toRational y1)
        ans = 
           fromRational 
              (rounder 3 $ 
              (toRational y2) * (g ^^ (xv-x2))) :: Double
        x1 = round (fromJust $ "x1" << rf) 
        x2 = round (fromJust $ "x2" << rf)
        xv = round (fromJust $ "xv" << rf)  
        y1 = round (fromJust $ "y1" << rf) 
        y2 = round (fromJust $ "y2" << rf)
        g  =  ((toRational y2)/(toRational y1)) ^. (1/toRational(x2-x1))
        a ^. b = fromMaybe 0 $ power a b