module FeedBackI where 
import Utils
import Data.List (minimumBy)


data SubTask = S {getSub :: Int} 
   deriving (Show, Read)

data Priority = P Int 
   deriving (Show, Read, Eq, Ord)

data FeedBack = FB
           { situation :: [String] -> Bool
           , feedBack  :: String
           , priority  :: Priority
           , subTask   :: SubTask
           }

best :: [String] -> [FeedBack] -> FeedBack
best rs = minimumBy ord . filter (flip (situation) rs) 
   where ord f1 f2 = compare (priority f1) (priority f2)

anyElem :: Eq a => [a] -> [a] -> Bool
anyElem as bs = any (`elem` bs) as 

anySub :: Ord a => [[a]] -> [a] -> Bool
anySub as a = any (`isSubSet` a) as

anyEq :: Ord a => [[a]] -> [a] -> Bool
anyEq as a = any (`setEq` a) as

fb0 :: [FeedBack]
fb0 =
   [ FB ([]==)

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)
     
   
   , FB (["round final answer correctly"]==)

        "Dit is correct, goed zo!"
       
        (P 1)

        (S 0)

   
    , FB (anySub 
         [["calculate: dx = x1", "calculate: dy = y1"]
         ,["calculate: dx = x2", "calculate: dy = y2"]])
       
         ("Het lijkt er op dat je gebruik gemaakt hebt van\n" ++ 
          "de verhouding tussen x en y om de gemiddelde\n" ++
          "verandering (het hellingsgetal) te berekenen,\n" ++
          "maar je moet toch juist kijken hoeveel y verandert\n" ++
          "als x met één stap toeneemt?")

        (P 2)

        (S 3)

   , FB (anyElem
        ["calculate: dx = x1"
        ,"calculate: dx = x2"
        ,"calculate: dy = y1"
        ,"calculate: dx = x1"])

        ("Kijk nog eens naar je berekening van de gemiddelde\n" ++ 
         "verandering (het hellingsgetal), heb je wel in\n" ++
         "teller en noemer met de veranderingen gewerkt?")

        (P 3)

        (S 3)

   , FB ("invert slope" `elem`)

        ("Het lijkt wel alsof je het aantal stappen in x gedeeld hebt\n" ++ 
         "door de verandering in y, dat moet toch juist andersom?")
        
        (P 4)

        (S 3)

   , FB ("negate slope" `elem`)

        ("Het lijkt wel alsof bij jou de gemiddelde verandering\n" ++ 
         "(het hellingsgetal) positief is in plaats van negatief\n" ++ 
         "of negatief in plaats van positief")
        
        (P 5)

        (S 3)

   , FB (anyElem
        ["calculate: dx1xv = xv"
        ,"calculate: dx2xv = xv"])

        ("Het lijkt er op dat je vergeten bent dat je gebruik moet\n" ++
         "maken van de toename in x tot aan de x-coordinaat van het \n" ++
         "vraagteken, om de gevraagde waarde van y te berekenen")

        (P 6)

        (S 2)

   , FB (anyElem
        ["calculate: inter = dx1xv * dy/dx"
        ,"calculate: inter = dx2xv * dy/dx"])

        ("Het lijkt er op dat je vergeten bent dat je vanaf één van\n" ++
         "de gegeven y-waardes verder moet rekenen om de gevraagde\n" ++ 
         "waarde van y te berekenen")

        (P 7)

        (S 2)

   , FB (anyElem
        ["calculate: inter = y2 + dx1xv * dy/dx"
        ,"calculate: inter = y1 + dx2xv * dy/dx"])

        ("Het lijkt er op dat je vanaf de verkeerde waarde van y aan\n" ++
         "het extrapoleren bent")

        (P 8)

        (S 2)

   , FB (anyElem
        ["calculate: inter = x1 + dx1xv * dy/dx"
        ,"calculate: inter = x2 + dx1xv * dy/dx"
        ,"calculate: inter = x1 + dx2xv * dy/dx"
        ,"calculate: inter = x2 + dx2xv * dy/dx"])

        ("Het lijkt er op dat je vanaf een waarde van x aan het\n" ++ 
         "extrapoleren bent, dat moet toch vanaf een waarde\n" ++
         "van y zijn?")

        (P 9)

        (S 2)

   , FB ("calculate y as a sequence" `elem`)
        
        ("Het lijkt wel alsof je alleen naar de y-waardes hebt gekeken,\n" ++
         "de waarden van x spelen toch ook een rol?")

        (P 10)

        (S 1)

   , FB ("negate final answer" `elem`)
        
        ("Het lijkt er op dat je eindanwoord positief is terwijl het\n" ++
         "negatief had moeten zijn of negatief terwijl het positief\n" ++ 
         "had moeten zijn.") 

        (P 11)

        (S 0)

   , FB ("round final answer incorrectly" `elem`)
        
        ("Heb je in je berekeningen wel goed afgerond?") 

        (P 12)

        (S 0)

   , FB ("round slope incorrectly" `elem`)
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) niet goed hebt afgerond, reken\n" ++
         "met de onafgeronde gemiddelde verandering")

        (P 13)

        (S 0)

   , FB ("round slope correctly" `elem`)
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) hebt afgerond, reken met de\n" ++
         "onafgeronde gemiddelde verandering")

        (P 14)

        (S 0)

   , FB ("round y-intercept incorrectly" `elem`)
        
        ("Het lijkt wel alsof je een formule hebt opgesteld,\n" ++
         "maar heb je het startgetal wel goed afgerond?")

        (P 15)

        (S 0)

   , FB ("round y-intercept correctly" `elem`)
        
        ("Het lijkt wel alsof je een formule hebt opgesteld\n" ++
         "en het startgetal hebt afgerond, reken met het\n" ++
         "onafageronde startgetal")

        (P 16)

        (S 0)

   , FB (const True)
        
        ("Je antwoord klopt niet, maar\n" ++
         "ik kan de fout niet herkennen")

        (P 17)

        (S 0)
   ]
       
fb3 :: [FeedBack]
fb3 =
   [ FB ([]==)

        "Dit is correct, goed zo!"
       
        (P 0)

        (S 0)
     
   
   , FB (["round slope correctly"]==)

        "Dit is correct, goed zo!"
       
        (P 1)

        (S 0)

   
    , FB (anySub 
         [["calculate: dx = x1", "calculate: dy = y1"]
         ,["calculate: dx = x2", "calculate: dy = y2"]])
       
         ("Het lijkt er op dat je gebruik gemaakt hebt van\n" ++ 
          "de verhouding tussen x en y om de gemiddelde\n" ++
          "verandering (het hellingsgetal) te berekenen,\n" ++
          "maar je moet toch juist kijken hoeveel y verandert\n" ++
          "als x met één stap toeneemt?")
       
        (P 2)

        (S 3)


   , FB ("invert slope" `elem`)

        ("Het lijkt wel alsof je het aantal stappen in x gedeeld hebt\n" ++ 
         "door de verandering in y, dat moet toch juist andersom?")
        
        (P 3)

        (S 3)

   , FB ("negate slope" `elem`)

        ("Het lijkt wel alsof bij jou de gemiddelde verandering\n" ++ 
         "(het hellingsgetal) positief is in plaats van negatief\n" ++ 
         "of negatief in plaats van positief")
        
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

   , FB ("round slope incorrectly" `elem`)
        
        ("Het lijkt er op dat je de gemiddelde verandering\n" ++
         "(het hellingsgetal) niet goed hebt afgerond")

        (P 6)

        (S 0)

   ]
