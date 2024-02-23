module FeedBack14 where 
import FeedBackI
import Data.Maybe
import Refinement

fb14 :: ParamFB 
fb14 rf =
   [ FB ([]==)

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)
     
   
   , FB (["round final answer correctly 3"]==)

        "Dit is correct, goed zo!"
       
        (P 1)

        (S 0)

   , FB (["round final answer correctly"]==)

        ("Rond af op drie decimalen en zorg ervoor" ++
        "dat je tussentijds niet afrondt")
       
        (P 2)

        (S 0)

   , FB (anySub [["calculate: f = y2-y1 or f = y1-y2", "calculate: g = f/dx"]])
       
        ("Het lijkt wel alsof je de groeifactor als hellingsgetal hebt berekend, "++ 
          "maar het is nu toch exponentieel?")

        (P 3)

        (S 3)

   , FB ("calculate: f = y1/x1 or f = y2/x2" `elem`)
       
        ("Het lijkt wel alsof je bij de groeifactor de verhouding tussen x en y hebt gebruikt, "++ 
          "maar het is nu toch exponentieel?")

        (P 4)

        (S 3)  

   , FB ("calculate: f = y2-y1 or f = y1-y2" `elem`)
       
        ("Het lijkt wel alsof bij de groeifactor het verschil in y waarden hebt gebruikt, "++ 
          "maar het is nu toch exponentieel?")

        (P 5)

        (S 3)  

   , FB (anyElem ["invert factor", "invert grow"])
       
        ("De groeifactor lijkt niet te kloppen. Is er sprake van toename of afname?")

        (P 6)

        (S 3)  

   , FB (anyElem ["calculate: dx = x1", "calculate: dx = x2"])
       
        ("Heb je de toename van x wel goed berekend?" ++ 
         " Hoeveel komt er bij x bij? ")

        (P 7)

        (S 3)

   , FB ("calculate: g = f/dx" `elem`)
       
        ("Het lijkt er op dat je de groeifactor per tijdseenheid met" ++ 
         " delen hebt berekend, klopt dat wel?")

        (P 8)

        (S 3)
   , FB ("calculate: g = f^dx" `elem`)
       
        ("Klopt de macht wel bij het berekenen van de groeifactor?" ++ 
         " Met die macht bereken je de groeifactor per stap van x")

        (P 9)

        (S 3)

   , FB (anySub [["calculate: g = f", "ng = g"]] )
       
        ("Het lijkt wel alsof je alleen naar de waarden van y gekeken hebt." ++ 
         " Maar de waarden van x spelen toch ook een rol?")

        (P 10)

        (S 1)

   , FB ("calculate: g = f" `elem`)
       
        ("Heb je bij het berekenen van de groeifactor wel omgerekend" ++
         " naar de groeifactor per stap van x?")

        (P 11)

        (S 3)

   , FB (anyElem ["calculate: dx1xv = xv", "calculate: dx2xv = xv"])
       
        ("Gerbruik je wel het verschil tussen een gegeven x-coördinaat" ++
         " en de x-coördinaat van de gevraagde waarde?")

        (P 12)

        (S 2)

   , FB (anyElem ["ng = g * dx1xv", "ng = g * dx2xv"])
       
        ("Het lijkt wel of je de groeifactor per stap vermenigvuldigd hebt met het aantal stappen." ++
         " Moet dat niet met machtsverheffen?")

        (P 13)

        (S 2)

   , FB ("ng = g" `elem`)
       
        ("Het lijkt wel alsof je vergeten bent dat je met de groeifactor" ++
         " een aantal stappen verder moet rekenen")

        (P 14)

        (S 2)

   , FB ("inter = bs + ng" `elem`)
       
        ("Het lijkt wel alsof je verder rekent door op te tellen." ++ 
         " Maar bij een exponentieel verband wordt toch elke stap vermenigvuldigd?")

        (P 15)

        (S 2)

   , FB ("no base used" `elem`)
       
        ("Het lijkt wel alsof je vergeten bent dat je vanaf" ++ 
         " een waarde van y verder moet rekenen.")

        (P 16)

        (S 2)

   , FB (anyElem ["base = y1", "base = y2"])
       
        ("Extrapoleer je wel vanaf de goede waarde van y?")

        (P 17)

        (S 2)
   
   , FB ("round final answer incorrectly" `elem`)
        
        ("Heb je in je berekeningen wel goed afgerond?") 

        (P 18)

        (S 1)

   , FB (anyElem
        ["round g correctly" 
        ,"round g correctly 3"])
        
        ("Het lijkt er op dat je de g groeifactor" ++
         " hebt afgerond, reken met de" ++
         " onafgeronde groeifactor")

        (P 19)

        (S 1)

   , FB ("round g incorrectly" `elem`)
        
        ("Het lijkt er op dat je de" ++
         " groeifactor niet goed hebt afgerond, reken" ++
         " met de onafgeronde groeifactor")

        (P 20)

        (S 1)

   , FB (==[""])
        
        ("Je antwoord klopt niet helemaal, maar " ++
         "ik kan de fout niet herkennen <br><br>" ++
         "Suggestie: maak een eenvoudigere deelopgave")

        (P 21)

        (S 1)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)