module FeedBack1 where 
import FeedBackI
import Data.Maybe
import Refinement

fb1 :: ParamFB
fb1 rf = [ FB ([]==)

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)

   , FB (["round final answer correctly 2"]==)

        "Dit is correct, goed zo!"
       
        (P 1)

        (S 0)
     
   , FB (["round final answer correctly"]==)

        ("Rond af op twee decimalen en zorg ervoor" ++
        "dat je tussentijds niet afrondt")
       
        (P 2)

        (S 0)

   
    , FB (anySub 
         [["calculate: dx = x1", "calculate: dy = y1"]
         ,["calculate: dx = x2", "calculate: dy = y2"]])
       
         ("Per stap van x is de verandering van y gelijk aan: " ++ show y2 ++ " - " ++ show y1 ++ 
          " = " ++ show (y2-y1) ++ ", toch?")

        (P 3)

        (S 3)

   , FB (anyElem
        ["calculate: dx = x1"
        ,"calculate: dx = x2"
        ,"calculate: dy = y1"
        ,"calculate: dx = x1"])

        ("Per stap van x is de verandering van y " ++ show y2 ++ " - " ++ show y1 ++ 
         " = " ++ show (y2-y1) ++ ", toch?")

        (P 4)

        (S 3)

   , FB ("invert slope" `elem`)

        ("Het lijkt wel alsof je het aantal stappen in x gedeeld hebt\n" ++ 
         "door de verandering in y, dat moet toch juist andersom?")
        
        (P 5)

        (S 3)

   , FB ("negate slope" `elem`)

        ("Het lijkt wel alsof bij jou de gemiddelde verandering\n" ++ 
         if y2 - y1 < 0 then 
         "(het hellingsgetal) positief is in plaats van negatief." else   
         "(het hellingsgetal) negatief is in plaats van positief.")
        
        (P 6)

        (S 3)

   , FB (anyElem
        ["calculate: dx1xv = xv"
        ,"calculate: dx2xv = xv"])

        ("Tussen " ++ show x2 ++ " en " ++ show xv ++ 
         " zitten " ++ show xv ++ " - " ++ show x2 ++
         " = " ++ show (xv-x2) ++ " stappen. Gerbuik dit om verder te rekenen.")

        (P 7)

        (S 2)

   , FB (anyElem
        ["calculate: inter = dx1xv * dy/dx"
        ,"calculate: inter = dx2xv * dy/dx"])

        ("Het lijkt er op dat je vergeten bent dat je vanaf " ++ 
         show y2 ++ " verder moet rekenen." )

        (P 8)

        (S 2)

   , FB (anyElem
        ["calculate: inter = y2 + dx1xv * dy/dx"
        ,"calculate: inter = y1 + dx2xv * dy/dx"])

        ("Kijk goed of je vanaf " ++ show y1 ++ " of vanaf " ++
        show y2 ++ " verder gaat rekenen.")

        (P 9)

        (S 2)

   , FB (anyElem
        ["calculate: inter = x1 + dx1xv * dy/dx"
        ,"calculate: inter = x2 + dx1xv * dy/dx"
        ,"calculate: inter = x1 + dx2xv * dy/dx"
        ,"calculate: inter = x2 + dx2xv * dy/dx"])

        ("Het lijkt er op dat je vanaf een waarde van x aan het\n" ++ 
         "extrapoleren bent, dat moet toch vanaf een waarde\n" ++
         "van y zijn?")

        (P 10)

        (S 2)

   , FB ("calculate y as a sequence" `elem`)
        
        ("Per stap van x is de verandandering van y gelijk aan <br>"
         ++ show y2 ++ " - " ++ show y1 ++ 
         " = " ++ show (y2-y1) ++ ".<br>" ++ 
         "Je moet dan toch " ++ show xv ++ " - " ++ show x2 ++ 
         " = " ++ show (xv-x2) ++ " stappen verder rekenen vanaf "
         ++ show y2++ "?")

        (P 11)

        (S 1)

   , FB ("negate final answer" `elem`)
        
        ("Het lijkt er op dat je eindantwoord " ++ 
            if (y2 - y1) * (xv - x2) + y2 < 0
            then "positief is terwijl het negatief had moeten zijn." 
            else "negatief is terwijl het positief had moeten zijn.") 

        (P 12)

        (S 1)

   , FB ("round final answer incorrectly" `elem`)
        
        ("Heb je in je berekeningen wel goed afgerond?") 

        (P 13)

        (S 1)

   , FB (anyElem
        ["round slope correctly" 
        ,"round slope correctly 2"])
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) hebt afgerond, reken met de\n" ++
         "onafgeronde gemiddelde verandering")

        (P 14)

        (S 1)

   , FB ("round slope incorrectly" `elem`)
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) niet goed hebt afgerond, reken\n" ++
         "met de onafgeronde gemiddelde verandering")

        (P 15)

        (S 1)


   , FB ("round y-intercept correctly" `elem`)
        
        ("Het lijkt wel alsof je een formule hebt opgesteld\n" ++
         "en het startgetal hebt afgerond, reken met het\n" ++
         "onafgeronde startgetal")

        (P 16)

        (S 0)

   , FB ("round y-intercept incorrectly" `elem`)
        
        ("Het lijkt wel alsof je een formule hebt opgesteld,\n" ++
         "maar heb je het startgetal wel goed afgerond?")

        (P 17)

        (S 0)

   , FB (==[""])
        
        ("Je antwoord klopt niet helemaal, maar " ++
         "ik kan de fout niet herkennen <br><br>" ++
         "Suggestie: bekijk de uitwerking")

        (P 18)

        (S 0)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)  