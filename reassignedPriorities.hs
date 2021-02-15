module ReassignedPriorities where

import Control.Monad

type Value = Int
type Index = Int

reassignedPriorities :: [Value] -> [Value]
reassignedPriorities priorities = do
    orderListbyIndex (rPriorities (orderListbyValue(gPrioritiesTuples priorities 1)))

gPrioritiesTuples :: [Value] -> Index -> [(Value,Index)]
gPrioritiesTuples [] _ = []
gPrioritiesTuples (x:xs) n = (x,n) : gPrioritiesTuples xs (n+1)

orderListbyValue [] = []
orderListbyValue (x:[]) = [x]
orderListbyValue xs = selectMinValue xs : orderListbyValue (remMinValue xs)
  where selectMinValue xs = head(filter (\(x,y) -> x == minimum(map fst xs)) xs)
        remMinValue xs = filter (\x -> x /= (selectMinValue xs)) xs

rPriorities list = rPriorities' list 1
rPriorities' [] _ = []
rPriorities' (x:[]) _ = [x]
rPriorities' (x:y:xs) n | fst x == fst y = x' : rPriorities' (y':xs) n
  | otherwise = x' : rPriorities' (y'':xs) (n+1)
  where x' = (n,snd x)
        y' = (n,snd y)
        y'' = (n+1, snd y)

orderListbyIndex xs = orderListbyIndex' xs 1
orderListbyIndex' [] _ = []
orderListbyIndex' (x:[]) _ = [fst x]
orderListbyIndex' xs n = fst (selectByIndex xs n) : orderListbyIndex' (remByIndex xs n) (n+1)
  where selectByIndex xs n = head(filter (\(x,y) -> y == n) xs)
        remByIndex xs n = filter (\(x,y) -> y /= n) xs