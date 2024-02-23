module FeedBack11 where 
import FeedBackI
import Data.Maybe
import Refinement

fb11 :: ParamFB 
fb11 rf =
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
       
        ("Het lijkt wel alsof je de groeifactor als hellingsgetal hebt berekend. "++ 
          "Bedenk dat per stap van x de waarde van y met de groeifactor wordt vermenigvuldigd")

        (P 3)

        (S 3)

   , FB ("calculate: f = y1/x1 or f = y2/x2" `elem`)
       
        ("Het lijkt wel alsof je bij de groeifactor de verhouding tussen x en y hebt gebruikt. "++ 
          "Bedenk dat per stap van x de waarde van y met de groeifactor wordt vermenigvuldigd")

        (P 4)

        (S 3)  

   , FB ("calculate: f = y2-y1 or f = y1-y2" `elem`)
       
        ("Het lijkt wel alsof bij de groeifactor het verschil in y waarden hebt gebruikt. "++ 
          "Bedenk dat per stap van x de waarde van y met de groeifactor wordt vermenigvuldigd")

        (P 5)

        (S 3)  

   , FB (anyElem ["invert factor", "invert grow"])
       
        ("De groeifactor lijkt niet te kloppen. Heb je wel de nieuwe" ++ 
          " y-waarde door de oude y-waarde gedeeld?")

        (P 6)

        (S 3)  

   , FB (anyElem ["calculate: dx = x1", "calculate: dx = x2"])
       
        ("Bij x komt er toch " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ 
          show (x2-x1) ++ " bij?")

        (P 7)

        (S 3)

   , FB (anyElem ["calculate: dx1xv = xv", "calculate: dx2xv = xv"])
       
        ("Bedenk dat je bijvoorbeeld vanaf " ++ show x2 ++ " nog " ++ 
          show (xv-x2) ++ " stappen verder kan rekenen tot " ++ show xv)

        (P 8)

        (S 2)

   , FB (anyElem ["ng = g * dx1xv", "ng = g * dx2xv"])
       
        ("Per stap wordt er vermenigvuldigd met de groeifactor g in, " ++ show (xv-x2) ++
         " stappen wordt dus in totaal vermenigvuldigd met: g^" ++ show (xv-x2)) 

        (P 9)

        (S 2)

   , FB ("ng = g" `elem`)
       
        ("Het lijkt wel alsof je vergeten bent dat je met de groeifactor g" ++
         " een aantal stappen verder moet rekenen." ++ 
         " Hier moet je: " ++ show (xv-x2) ++ " stappen verder rekenen")

        (P 10)

        (S 2)

   , FB ("inter = bs + ng" `elem`)
       
        ("Het lijkt wel alsof je verder rekent door op te tellen." ++
         " Bij exponentiële verbanden wordt elke stap vermenigvuldigd." ++
         " Bereken de gevraagde waarde dus met vermenigvuldigen." ++ 
         " Zoals: " ++ show y2 ++ " * g^" ++ show (xv-x2) ++ 
         ", waarbij g de groeifactor is")

        (P 11)

        (S 2)

   , FB ("no base used" `elem`)
       
        ("Het lijkt wel alsof je vergeten bent dat je vanaf een waarde van y" ++ 
         " verder moet rekenen. Bijvoorbeeld: " ++ show y2)

        (P 12)

        (S 2)

   , FB (anyElem ["base = y1", "base = y2"])
       
        ("Extrapoleer je wel vanaf de goede waarde van y?")

        (P 13)

        (S 2)
   
   , FB ("round final answer incorrectly" `elem`)
        
        ("Heb je in je berekeningen wel goed afgerond?" ++
         " Rond tussentijds niet af") 

        (P 14)

        (S 1)

   , FB (anyElem
        ["round g correctly" 
        ,"round g correctly 3"])
        
        ("Rond tussentijds niet af")

        (P 15)

        (S 1)

   , FB ("round g incorrectly" `elem`)
        
        ("Rond tussentijds niet af")

        (P 16)

        (S 1)

   , FB (==[""])
        
        ("Je antwoord klopt niet helemaal, maar " ++
         "ik kan de fout niet herkennen <br><br>" ++
         "Suggestie: maak een eenvoudigere deelopgave")

        (P 17)

        (S 1)
   ]
   where 
      x1 = round (fromJust $ "x1" << rf) 
      x2 = round (fromJust $ "x2" << rf) 
      y1 = round (fromJust $ "y1" << rf) 
      y2 = round (fromJust $ "y2" << rf) 
      xv = round (fromJust $ "xv" << rf)