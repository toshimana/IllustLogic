module Lib
    ( Constraint
    , createCandidates_
    , volume
    ) where

import Data.List as L (replicate, null, intersperse)

type Constraint = [Int]

createCandidates_ :: Int -> Constraint -> [[Bool]]
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

volume :: Constraint -> Int
volume = sum . (intersperse 1)

