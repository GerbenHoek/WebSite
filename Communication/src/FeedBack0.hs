module FeedBack0 where 
import FeedBackI
import Data.Maybe
import Refinement

fb0 :: ParamFB 
fb0 rf =
   [ FB ([]==)

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

    , FB ("calculate: dx = 1" `elem`)
       
         ("Het lijkt er op dat je bent uitgegaan van een toename in x van 1 stap.<br>" ++
          "Maar de verandering van y moet je nu toch verdelen over<br>" ++ show x2 ++ " - " ++ show x1 ++
          " = " ++ show (x2 - x1) ++ " stappen?")

        (P 3)

        (S 3)

     , FB (anySub 
         [["calculate: dx = x1", "calculate: dy = y1"]
         ,["calculate: dx = x2", "calculate: dy = y2"]])
       
         ("Het lijkt er op dat je gebruik gemaakt hebt van\n" ++ 
          "de verhouding tussen x en y om de gemiddelde\n" ++
          "verandering (het hellingsgetal) te berekenen,\n" ++
          "maar je moet toch juist kijken hoeveel y verandert\n" ++
          "als x met één stap toeneemt?")

        (P 4)

        (S 3)

   , FB (anyElem
        ["calculate: dx = x1"
        ,"calculate: dx = x2"
        ,"calculate: dy = y1"
        ,"calculate: dx = x1"])

        ("Kijk nog eens naar je berekening van de gemiddelde\n" ++ 
         "verandering (het hellingsgetal), heb je wel in\n" ++
         "teller en noemer met de veranderingen gewerkt?")

        (P 5)

        (S 3)

   , FB ("invert slope" `elem`)

        ("Het lijkt wel alsof je het aantal stappen in x gedeeld hebt\n" ++ 
         "door de verandering in y, klopt dat wel?")
        
        (P 6)

        (S 3)

   , FB ("negate slope" `elem`)

        ("De gemiddelde verandering (het hellingsgetal) klopt niet." ++ 
         " Is er sprake van toename of afname?")
        
        (P 7)

        (S 3)

   , FB (anyElem
        ["calculate: dx1xv = xv"
        ,"calculate: dx2xv = xv"])

        ("Gebruik je wel het verschil tussen een gegeven x-coördinaat" ++
         " en de x-coördinaat van de gevraagde waarde?")

        (P 8)

        (S 2)

   , FB (anyElem
        ["calculate: inter = dx1xv * dy/dx"
        ,"calculate: inter = dx2xv * dy/dx"])

        ("Het lijkt er op dat je vergeten bent dat je vanaf één van\n" ++
         "de gegeven y-waardes verder moet rekenen om de gevraagde\n" ++ 
         "waarde van y te berekenen")

        (P 9)

        (S 2)

   , FB (anyElem
        ["calculate: inter = y2 + dx1xv * dy/dx"
        ,"calculate: inter = y1 + dx2xv * dy/dx"])

        ("Het lijkt er op dat je vanaf de verkeerde waarde van y aan\n" ++
         "het extrapoleren bent")

        (P 10)

        (S 2)

   , FB (anyElem
        ["calculate: inter = x1 + dx1xv * dy/dx"
        ,"calculate: inter = x2 + dx1xv * dy/dx"
        ,"calculate: inter = x1 + dx2xv * dy/dx"
        ,"calculate: inter = x2 + dx2xv * dy/dx"])

        ("Het lijkt er op dat je vanaf een waarde van x aan het\n" ++ 
         "extrapoleren bent, dat moet toch vanaf een waarde\n" ++
         "van y zijn?")

        (P 11)

        (S 2)

   , FB ("calculate y as a sequence" `elem`)
        
        ("Het lijkt wel alsof je alleen naar de y-waardes hebt gekeken,\n" ++
         "de waarden van x spelen toch ook een rol?")

        (P 12)

        (S 1)

   , FB ("negate final answer" `elem`)
        
        ("Het lijkt er op dat je eindantwoord " ++ 
            if (y2 - y1) * (xv - x2) + y2 < 0
            then "positief is terwijl het negatief had moeten zijn." 
            else "negatief is terwijl het positief had moeten zijn.") 

        (P 13)

        (S 1)

   , FB ("round final answer incorrectly" `elem`)
        
        ("Heb je in je berekeningen wel goed afgerond?" ++ 
         " Rond niet tussentijds af.") 

        (P 14)

        (S 1)

   , FB (anyElem
        ["round slope correctly" 
        ,"round slope correctly 2"])
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) hebt afgerond, reken met de\n" ++
         "onafgeronde gemiddelde verandering")

        (P 15)

        (S 1)

   , FB ("round slope incorrectly" `elem`)
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) niet goed hebt afgerond, reken\n" ++
         "met de onafgeronde gemiddelde verandering")

        (P 16)

        (S 1)


   , FB ("round y-intercept correctly" `elem`)
        
        ("Het lijkt wel alsof je een formule hebt opgesteld\n" ++
         "en het startgetal hebt afgerond, reken met het\n" ++
         "onafgeronde startgetal")

        (P 17)

        (S 0)

   , FB ("round y-intercept incorrectly" `elem`)
        
        ("Het lijkt wel alsof je een formule hebt opgesteld,\n" ++
         "maar heb je het startgetal wel goed afgerond?")

        (P 18)

        (S 0)

   , FB (==[""])
        
        ("Je antwoord klopt niet helemaal, maar " ++
         "ik kan de fout niet herkennen <br><br>" ++
         "Suggestie: maak een eenvoudigere deelopgave")

        (P 19)

        (S 1)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)