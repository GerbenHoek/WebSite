module InterStrat where 
import Utils
import Trails
import StrategyBas
import Rules
import Data.Maybe
import InterRules
import Refinement

lbl :: String -> Rule (Ref Rational)
lbl = label allRules

caldx :: Strat (Ref Rational)
caldx = R (lbl "calculate: dx = x2 - x1") <|>
        R (lbl "calculate: dx = x1") <|>
        R (lbl "calculate: dx = x2") 

caldy :: Strat (Ref Rational)
caldy = R (lbl "calculate: dy = y2 - y1") <|>
        R (lbl "calculate: dy = y1") <|>
        R (lbl "calculate: dy = y2") 

caldyDdx :: Strat (Ref Rational)
caldyDdx = R (lbl "calculate: slope = dy/dx") .*.
           option (R (lbl "negate slope")) .*.
           option (R (lbl "invert slope")) .*.
           option (R (lbl "round slope correctly") <|>
                   R (lbl "round slope incorrectly"))

caldx1v :: Strat (Ref Rational)
caldx1v = R (lbl "calculate: dx1xv = xv - x1") <|>
          R (lbl "calculate: dx1xv = xv")

caldx2v :: Strat (Ref Rational)
caldx2v = R (lbl "calculate: dx2xv = xv - x2") <|>
          R (lbl "calculate: dx2xv = xv")

interpol1 :: Strat (Ref Rational)
interpol1 = R (lbl "calculate: inter = y1 + dx1xv * dy/dx") <|>
            R (lbl "calculate: inter = y2 + dx1xv * dy/dx") <|>
            R (lbl "calculate: inter = dx1xv * dy/dx") <|>
            R (lbl "calculate: inter = x1 + dx1xv * dy/dx") <|>
            R (lbl "calculate: inter = x2 + dx1xv * dy/dx") 

interpol2 :: Strat (Ref Rational)
interpol2 = R (lbl "calculate: inter = y2 + dx2xv * dy/dx") <|>
            R (lbl "calculate: inter = y1 + dx2xv * dy/dx") <|>
            R (lbl "calculate: inter = dx2xv * dy/dx") <|> 
            R (lbl "calculate: inter = x1 + dx2xv * dy/dx") <|>
            R (lbl "calculate: inter = x2 + dx2xv * dy/dx") 

interFromCept :: Strat (Ref Rational)
interFromCept = (R (lbl "calculate: b = y1 - x1 * dy/dx") <|>
                 R (lbl "calculate: b = y2 - x2 * dy/dx")) .*. 
                option (R (lbl "round y-intercept correctly") <|>
                        R (lbl "round y-intercept incorrectly")) .*.
                        R (lbl "calculate: inter = b + xv * dyDdx")

finalRounding :: Strat (Ref Rational)
finalRounding = 
   option (R (lbl "round final answer correctly") <|> 
           R (lbl "round final answer incorrectly"))

negateFinalAns :: Strat (Ref Rational)
negateFinalAns = option (R (lbl "negate final answer"))

interpolation :: Strat (Ref Rational)
interpolation = 
   (caldx .*. 
   caldy .*. 
   caldyDdx .*. 
   ((caldx1v .*. interpol1) <|> 
   (caldx2v .*. interpol2) <|>
   (interFromCept)) <|> R calSeq) .*. 
   finalRounding .*. 
   negateFinalAns .*.
   toFFI

trunkStrat :: Strat (Ref Rational)
trunkStrat = 
   caldx .*. caldy .*. caldyDdx .*. R finalSlope

finalSlope :: Rule (Ref Rational)
finalSlope = rule "finSlope" f 
   where 
      f rf = 
         case (filter (("dy/dx" ==) . fst) rf)
           of [] -> []
              a  -> [a]

rightStrat :: Strat (Ref Rational)
rightStrat = 
   try (R (lbl "calculate: dx = x2 - x1")) .*.
   try (R (lbl "calculate: dy = y2 - y1")) .*.
   try (R (lbl "calculate: slope = dy/dx")) .*.
   ((noX2 .*. try (R (lbl "calculate: dx1xv = xv - x1")) .*. 
   try (R (lbl "calculate: inter = y1 + dx1xv * dy/dx"))) <|> 
   (noX1 .*. try (R (lbl "calculate: dx2xv = xv - x2")) .*. 
   try (R (lbl "calculate: inter = y2 + dx2xv * dy/dx"))))
   where 
      noX1 = filterS (not.isPresent "dx1xv") "not via x1"
      noX2 = filterS (not.isPresent "dx2xv") "not via x2"

toFinalFormI' :: Ref Rational -> [(Ref Rational)]
toFinalFormI' rf = 
   case (filter (("inter" ==) . fst) rf)
     of []   -> []
        intr -> [intr]

toFFI :: Strat (Ref Rational)
toFFI = R $ rule "toFinal" toFinalFormI'

toFinalFormI :: Ref Rational -> Ref Rational -> [(Ref Rational)]
toFinalFormI st rf = 
   case applyAll rightStrat (rf ++ st)
     of [] -> []
        r:rs -> [(filter (("inter" ==) . fst) r)] 
        