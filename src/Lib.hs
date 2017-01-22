module Lib
    ( Constraint
    , createCandidates
    , volume
    ) where

import Data.List as L (replicate, null, intersperse)

type Constraint = ([Int],(Int,Int))

createCandidates :: Constraint -> [[Bool]]
createCandidates (cs,(lb,ub)) = createCandidates_ (ub-lb+1) cs

createCandidates_ :: Int -> [Int] -> [[Bool]]
createCandidates_ num [] = [L.replicate num False]
createCandidates_ num constraint@(x:xs) =
    case compare num (volume constraint) of
      LT -> []
      EQ -> if L.null xs then [L.replicate x True] else blackList
      GT -> blackList ++ whiteList
     where 
       blackList = map (\n -> f x (False:n)) $ createCandidates_ (num-x-1) xs
       whiteList = map (\n -> False:n) $ createCandidates_ (num-1) constraint
       f 0 xs = xs
       f n xs = True:(f (n-1) xs)

volume :: [Int] -> Int
volume = sum . (intersperse 1)

