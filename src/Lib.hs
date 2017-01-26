module Lib
    ( Cell
    , Index
    , Constraint
    , Constraints
    , adaptLine
    , labeling
    , createCandidates
    , solveConstraint
    , volume
    ) where

import Data.List as L (replicate, null, intersperse, zipWith)

type Cell = Maybe Bool
type Index = (Int, Int)
type Constraint = ([Int],(Int,Int))
type Constraints = [Constraint]

adaptLine :: [Cell] -> [Bool] -> Bool
adaptLine line xs = 
    and $ L.zipWith f line xs
        where
          f (Just True) False = False
          f (Just False) True = False
          f _ _ = True

labeling :: [Bool] -> [Int]
labeling list = f list 0 
    where
      f [] cur = []
      f (x:xs) cur = if (odd cur) == x then cur : (f xs cur) else (cur+1):(f xs (cur+1))

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

match :: [Int] -> [Int] -> [Maybe Int]
match [] _ = []
match _ [] = []
match (x:xs) (y:ys) = let z = if x == y then Just x else Nothing in z : (match xs ys)

solveConstraint :: [(Index,Cell)] -> Constraint -> Maybe ([(Index,Cell)], Constraints)
solveConstraint cells constraint@(xs,(lb,ub)) = 
    if null candidates then Nothing
    else Just (newCells,[constraint]) 
    where
        targetCells = drop (lb-1) $ take (ub) cells
        targets = map snd targetCells
        len = ub - lb + 1
        candidates = labeling $ head $ filter (adaptLine targets) (createCandidates_ len xs)
        revCandidates = labeling $ reverse $ head $ filter (adaptLine (reverse targets)) (createCandidates_ len (reverse xs))
        line = match candidates revCandidates
        newline = map (maybe Nothing (\n -> Just (odd n)) ) line
        newCells = foldl (\cur -> \((i,c),n) -> if c /= n then (i,n):cur else cur) [] (zip targetCells newline)



