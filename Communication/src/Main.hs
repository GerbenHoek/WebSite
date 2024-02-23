--   request:
--   { "service" : "feedback"
--   , "number" : 2
--   , "task" : {"x1": 1, "x2": 2, "x3": 5, "y1": 8, "y2": 10}
--   , "answer": 10
--   }
--    
--   response:
--   
--   { "feedback": "Let op: invert slope"
--   , "number" : 1
--   }
--
-------------------------------------------------------------------
--
--   request:
--   { "service" : "task"
--   , "number" : 2
--   }
--   
--   response:
--   
--   { "task" : {"x1": 1, "x2": 2, "x3": 5, "y1": 8, "y2": 10}
--   }
------------------------------
--
--   request:
--   { "service" : "workout"
--   , "number" : 2
--   , "task" : {"x1": 1, "x2": 2, "x3": 5, "y1": 8, "y2": 10}
--   }
--   
--   response:
--   
--   { "workout" : "this is the answer"
--   }

-- http://localhost/mbt-server.cgi?input={"task":{"x1":1,"x2":2,"x3":5,"y1":8,"y2":10},"answer":42}
-- https://ideastest.science.uu.nl/cgi-bin/mbt-server.cgi?input={"task":{"x1":1,"x2":2,"x3":5,"y1":8,"y2":10},"answer":42}

module Main where

import Data.List
import Ideas.Text.JSON
import Network.CGI
import Parameter
import Calculate 
import Utils (floatToR)
import Control.Monad.Trans.Class (lift)
import WorkOut

cgiMain :: CGI CGIResult
cgiMain = do
   setHeader "Content-type" "text/json"
   setHeader "Access-Control-Allow-Origin" "*"
   mtxt  <- getInput "input"
   tsks  <- lift loadTasks
   bgs'  <- lift $ readFile "./buggyRules/buggys"
   bgsE' <- lift $ readFile "./buggyRules/buggysE"
   let bgs  = read bgs'  :: [String]
   let bgsE = read bgsE' :: [String]
   case mtxt of
      Nothing  -> fail "no input"
      Just txt -> 
         case parseJSON txt of
            Left err -> fail $ "invalid json: " ++ show err
            Right json -> 
               case evalDecoderJSON decS json of
                  Left err -> fail $ "invalid json: " ++ show err
                  Right s  -> 
                     case s of 
                        "feedback" -> output $ show $ fbf json bgs bgsE
                        "task"     -> output $ show $ tkf json tsks
                        "workout"  -> output $ show $ wkf json 

fbf :: JSON -> [String] -> [String] -> JSON
fbf json bgs bgsE = case evalDecoderJSON decF json of  
                   Left err -> error "tkf: invalid json"
                   Right (n, t, r) | n < 10    -> responseF (calculate bgs n t r) 
                                   | otherwise -> responseF (calculate bgsE n t r) 

tkf :: JSON -> [[(String, Rational)]] -> JSON
tkf json tsks = case evalDecoderJSON decN json of  
                   Left err -> error "tkf: invalid json"
                   Right n | n < 10    -> responseT $ tsks!!n
                           | otherwise -> responseT $ tsks!!(n-6)

wkf :: JSON -> JSON
wkf json = case evalDecoderJSON decW json of 
              Left err -> error "tkf: invalid json"
              Right (n, t) -> responseW $ wkSelect n t  


decS :: DecoderJSON String
decS = jObject $ do
   service <- jKey "service" jString
   return (service)

decF :: DecoderJSON (Int, [(String, Rational)], Rational)
decF = jObject $ do 
   number  <- jKey "number" $ jInt
   task    <- jKey "task" $ jObject $ jWithKeys (\k -> 
                (\v -> (k, v)) <$> jRational)
   answer  <- jKey "answer" jRational
   return (number, task, answer)

decW :: DecoderJSON (Int, [(String, Rational)])
decW = jObject $ do 
   number  <- jKey "number" $ jInt
   task    <- jKey "task" $ jObject $ jWithKeys (\k -> 
                (\v -> (k, v)) <$> jRational)
   return (number, task)

decN :: DecoderJSON Int
decN = jObject $ do 
   number  <- jKey "number" $ jInt
   return number

responseF :: (String, Int) -> JSON
responseF (s, k) = 
   Object [("feedback", String s)
          ,("number", Integer $ fromIntegral k)]

responseT :: [(String, Rational)] -> JSON
responseT rf = 
   Object [("task", 
      Object [(s, Double $ fromRational r) | (s, r) <- rf])]

responseW :: String -> JSON
responseW s = Object [("workout", String s)]

jNumber :: DecoderJSON Double
jNumber = jDouble <|> fromIntegral <$> jInt

jRational :: DecoderJSON Rational
jRational = convert <$> (jDouble <|> fromIntegral <$> jInt)

convert :: Double -> Rational 
convert = floatToR . show 

main :: IO ()
main = runCGI (handleErrors cgiMain)

