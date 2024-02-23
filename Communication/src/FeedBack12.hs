module FeedBack12 where 
import FeedBackI
import Data.Maybe
import Refinement

fb12 :: ParamFB 
fb12 rf =
   [ FB (anyEq [ []
               , ["round final answer correctly 3"]
               , ["round g correctly 3"]
               , ["round g correctly 3", "round final answer correctly 3"]
               ])

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)

   , FB (["round final answer correctly"]==)

        ("Rond af op drie decimalen en zorg ervoor" ++
        "dat je tussentijds niet afrondt")
       
        (P 1)

        (S 0)


   , FB ("calculate: f = y1/x1 or f = y2/x2" `elem`)
       
        ("Het lijkt wel alsof je bij de groeifactor de verhouding tussen x en y hebt gebruikt. "++ 
          "De groeifactor is hier in de vraagstelling gegeven")

        (P 2)

        (S 3)   

   , FB (anyElem ["invert factor", "invert grow"])
       
        ("Reken je verder, of reken je terug?")

        (P 3)

        (S 3)  

   , FB (anyElem ["calculate: dx1xv = xv", "calculate: dx2xv = xv"])
       
        ("Bedenk dat je vanaf " ++ show x2 ++ " nog " ++ 
          show (xv-x2) ++ " stappen verder moet rekenen tot " ++ show xv)

        (P 4)

        (S 2)

   , FB (anyElem ["ng = g * dx1xv", "ng = g * dx2xv"])
       
        ("Reken verder door de groeifactor g tot de macht het aantal stappen te doen. " ++
         "Reken verder met g^" ++ show (xv-x2))

        (P 5)

        (S 2)

   , FB ("ng = g" `elem`)
       
        ("Het lijkt wel alsof je vergeten bent dat je met de groeifactor g" ++
         " een aantal stappen verder moet rekenen." ++ 
         " Hier moet je: " ++ show (xv-x2) ++ " stappen verder rekenen")

        (P 6)

        (S 2)

   , FB ("inter = bs + ng" `elem`)
       
        ("Het lijkt wel alsof je verder rekent door op te tellen." ++ 
         " Bereken de gevraagde waarde met vermenigvuldigen." ++ 
         " Zoals: " ++ show y2 ++ " * g^" ++ show (xv-x2) ++ 
         ", waarbij g de groeifactor is")

        (P 7)

        (S 2)

   , FB ("no base used" `elem`)
       
        ("Het lijkt wel alsof je vergeten bent dat je vanaf " ++ 
         show y2 ++ " verder moet rekenen.")

        (P 8)

        (S 2)

   , FB (anyElem ["base = y1", "base = y2"])
       
        ("Extrapoleer je wel vanaf de goede waarde van y?")

        (P 9)

        (S 2)
   
   , FB ("round final answer incorrectly" `elem`)
        
        ("Heb je in je berekeningen wel goed afgerond?" ++
         " Rond tussentijds niet af") 

        (P 10)

        (S 1)

   , FB (anyElem
        ["round g correctly" 
        ,"round g correctly 3"])
        
        ("Rond tussentijds niet af")

        (P 11)

        (S 1)

   , FB ("round g incorrectly" `elem`)
        
        ("Rond tussentijds niet af")

        (P 12)

        (S 1)

   , FB (==[""])
        
        ("Je antwoord klopt niet helemaal, maar " ++
         "ik kan de fout niet herkennen <br><br>" ++
         "Suggestie: maak een eenvoudigere deelopgave")

        (P 13)

        (S 1)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)