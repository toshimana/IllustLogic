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
    , createNewLine
    ) where

import Data.List as L (replicate, null, intersperse, zipWith)
import Data.Maybe

type Cell = Maybe Bool
type Index = (Int, Int)
type Constraint = ([Int],(Int,Int))
type Constraints = [Constraint]

adaptLine :: [Cell] -> [Bool] -> Bool
adaptLine line xs = and $ L.zipWith f line xs
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
solveConstraint cells constraint@(xs,(lb,ub)) = if null c then Nothing else Just (newCells,newConstraint) 
    where
        targetCells = drop (lb-1) $ take (ub) cells
        targets = map snd targetCells
        len = ub - lb + 1
        c = filter (adaptLine targets) (createCandidates_ len xs)
        candidates = labeling $ head c 
        revCandidates = labeling $ reverse $ head $ filter (adaptLine (reverse targets)) (createCandidates_ len (reverse xs))
        line = match candidates revCandidates
        newline = map (maybe Nothing (\n -> Just (odd n)) ) line
        newCells = foldl (\cur -> \((i,c),n) -> if isJust n && c /= n  then (i,n):cur else cur) [] (zip targetCells newline)
        l = map (\(n,i) -> (div (fromJust n) 2,i)) $ filter (maybe False even . fst) $ zip line [lb..ub]
        newConstraint = filter (\(n,(lb,ub))-> volume n /= (ub-lb+1)) $ filter (not.null.fst) $ createNewConstraint constraint l
        createNewConstraint xs [] = [xs]
        createNewConstraint (xs,(lb,ub)) ((c,i):cs) = let (a,b) = splitAt c xs in (a,(lb,i-1)) : createNewConstraint (b,(i+1,ub)) (map (\(n,j) -> (n-c,j)) cs)

createNewLine :: [(Index,Cell)] -> Constraints -> Maybe ([(Index,Cell)], Constraints)
createNewLine lineCell constraints = 
    let ret = map (solveConstraint lineCell) constraints in
    if any isNothing ret then Nothing
    else let a = map fromJust ret in 
         Just (concat $ map fst a, concat $ map snd a)

