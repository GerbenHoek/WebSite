module FeedBack3 where 
import FeedBackI
import Data.Maybe
import Refinement

fb3 :: ParamFB
fb3 rf = 
   [ FB ([]==)

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)
     
   
   , FB (["round slope correctly 2"]==)

        "Dit is correct, goed zo!"
       
        (P 1)

        (S 0)

   , FB (["round slope correctly"]==)

        ("Rond af op twee decimalen en zorg ervoor" ++
        "dat je tussentijds niet afrondt")
       
        (P 2)

        (S 0)

    , FB (anySub 
         [["calculate: dx = x1", "calculate: dy = y1"]
         ,["calculate: dx = x2", "calculate: dy = y2"]])
       
         (show y1 ++ "/" ++ show x1 ++ " is niet gelijk aan " ++ 
          show y2 ++ "/" ++ show x2 ++ "." ++ 
          " Je hebt hier niet te maken met een verhoudingstabel." ++ 
          " Bereken de gemiddelde verandering door de verandering in y" ++ 
          " te delen door de toename in x.")
       
        (P 3)

        (S 3)

   , FB ("invert slope" `elem`)

        ("Het lijkt wel alsof je het aantal stappen in x gedeeld hebt\n" ++ 
         "door de verandering in y, dat moet toch juist andersom?")
        
        (P 4)

        (S 3)

   , FB ("negate slope" `elem`)

        ("Er is toch sprake van " ++
         if signum (y2-y1) == signum (x2-x1) 
         then "toename?"
         else "afname?" )
        
        (P 5)

        (S 3)

   , FB (anyElem
        ["calculate: dx = x1"
        ,"calculate: dx = x2"
        ,"calculate: dy = y1"
        ,"calculate: dx = x1"])

        ("Je moet toch " ++ show y1 ++ " - " ++ show y2 ++ 
         " delen door "++ show x1 ++ " - " ++ show x2 ++ "?")

        (P 6)

        (S 3)

   , FB ("round slope incorrectly" `elem`)
        
        ("Heb je wel goed afgerond?")

        (P 7)

        (S 0)

   , FB (==[""])
        
        ("Je antwoord klopt niet helemaal, maar " ++
         "ik kan de fout niet herkennen <br><br>" ++
         "Suggestie: bekijk de uitwerking")
        
        (P 7)

        (S 0)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)  
