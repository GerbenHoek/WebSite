module FeedBack13 where 
import FeedBackI
import Data.Maybe
import Refinement

fb13 :: ParamFB 
fb13 rf =
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
         " Maar het verband tussen x en y is exponentieel." ++
         " Bereken eerst met welke facor " ++ show y1 ++ " vermenigvildig moet worden zodat" ++
         " de uitkomst " ++ show y2 ++ " is. Bereken daarna de groeifactor per stap van x")

        (P 3)

        (S 3)

   , FB ("calculate: f = y1/x1 or f = y2/x2" `elem`)
       
        ("Het lijkt wel alsof je bij de groeifactor de verhouding tussen x en y hebt gebruikt. "++ 
         " Maar het verband tussen x en y is exponentieel." ++
         " Bereken eerst met welke facor " ++ show y1 ++ " vermenigvildig moet worden zodat" ++
         " de uitkomst " ++ show y2 ++ "is. Bereken daarna de groeifactor per stap van x")

        (P 4)

        (S 3)  

   , FB ("calculate: f = y2-y1 or f = y1-y2" `elem`)
       
        ("Het lijkt wel alsof bij de groeifactor het verschil in y waarden hebt gebruikt. " ++ 
         " Maar het verband tussen x en y is exponentieel." ++
         " Bereken eerst met welke facor " ++ show y1 ++ " vermenigvuldigd moet worden zodat" ++
         " de uitkomst " ++ show y2 ++ " is. Bereken daarna de groeifactor per stap van x")

        (P 5)

        (S 3)  

   , FB (anyElem ["invert factor", "invert grow"])
       
        ("De groeifactor lijkt niet te kloppen. Heb je in je berekening wel de nieuwe" ++ 
          " y-waarde door de oude y-waarde gedeeld?")

        (P 6)

        (S 3)  

   , FB (anyElem ["calculate: dx = x1", "calculate: dx = x2"])
       
        ("Bij x komt er toch " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ 
          show (x2-x1) ++ " bij?")

        (P 7)

        (S 3)

   , FB ("calculate: g = f/dx" `elem`)
       
        ("Het lijkt er op dat je de groeifactor per tijdseenheid met delen hebt berekend." ++
         " De groeifactor per stap van x bereken je toch met machtsverheffen?")

        (P 8)

        (S 3)

   , FB ("calculate: g = f^dx" `elem`)
       
        ("Klopt de macht wel bij het berekenen van de groeifactor?" ++ 
         " Met die macht bereken je de groeifactor per stap van x." ++
         " Gebruik dus 1/" ++ show (x2 - x1))

        (P 9)

        (S 3)

   , FB ("calculate: g = f" `elem`)
       
        ("Heb je bij het berekenen van de groeifactor wel omgerekend" ++
         " naar de groeifactor per stap van x?" ++
         " Hiervoor gebruik je dat er bij x " ++ show x2 ++ " - " ++ show x1 ++ " = " ++ 
          show (x2-x1) ++ " bijkomt" )

        (P 11)

        (S 3)
   
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