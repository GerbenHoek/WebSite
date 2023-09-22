module Refinement where 
import Data.Maybe

type Ref a = [(String, a)]

replace :: (String, a) -> Ref a -> Ref a
replace _ [] = []
replace (str, a) ((str', a'):rf) =
    if str == str' then (str, a):replace (str, a) rf  
    else (str', a'):replace (str, a) rf

instance Num a => Num (Maybe a) where 
    a + b = (+) <$> a <*> b
    a - b = (-) <$> a <*> b
    a * b = (*) <$> a <*> b
    fromInteger a = Just $ fromInteger a
    abs a = abs <$> a
    signum a = signum <$> a

instance Fractional a => Fractional (Maybe a) where 
    a / b = (/) <$> a <*> b
    fromRational r = Just $ fromRational r

isPresent :: String -> Ref a -> Bool
isPresent str = elem str . map fst 

--prepend only if the second coordinate is not Nothing 
(|:) :: (String, Maybe a) -> Ref a-> [Ref a]
(str, mn) |: rf =
   case mn
     of Nothing -> []
        Just n  -> [(str, n):rf]

--replace item 
(!:) :: (String, Maybe a) -> Ref a -> [Ref a]
(str, mn) !: rf = 
   case mn
     of Nothing -> []
        Just n  -> [replace (str, n) rf]

-- take item out if the refinement
(<<) :: String -> Ref a -> Maybe a
str << rf = lookup str rf