module FeedBackI where 
import Utils
import Refinement
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

type ParamFB = Ref Rational -> [FeedBack]

best :: [String] -> [FeedBack] -> FeedBack
best rs = minimumBy ord . filter (flip (situation) rs) 
   where ord f1 f2 = compare (priority f1) (priority f2)

anyElem :: Eq a => [a] -> [a] -> Bool
anyElem as bs = any (`elem` bs) as 

anySub :: Ord a => [[a]] -> [a] -> Bool
anySub as a = any (`isSubSet` a) as

anyEq :: Ord a => [[a]] -> [a] -> Bool
anyEq as a = any (`setEq` a) as
       