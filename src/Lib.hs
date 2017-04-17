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

import Data.List as L (replicate, null, intersperse, zipWith, foldl', partition)
import Data.Maybe
import Data.Set as T

type Cell = Maybe Bool
type Index = (Int, Int)
type Constraint = ([Int],(Int,Int))
type Constraints = [Constraint]

adaptLine :: [Cell] -> [Bool] -> Bool
adaptLine line xs = and $ L.zipWith f line xs
        where
          f (Just a) b = a==b
          f _ _ = True

labeling :: [Bool] -> [Int]
labeling list = f list 0 
    where
      f [] cur = []
      f (x:xs) cur = if (odd cur) == x then cur : (f xs cur) else (cur+1):(f xs (cur+1))

createCandidates :: Constraint -> [[Bool]]
createCandidates (cs,(lb,ub)) = createCandidates_ (ub-lb+1) cs (volume cs)

createCandidates_ :: Int -> [Int] -> Int -> [[Bool]]
createCandidates_ num [] _ = [L.replicate num False]
createCandidates_ num constraint@(x:xs) vol =
    case compare num vol of
      LT -> []
      EQ -> if L.null xs then [L.replicate x True] else blackList
      GT -> blackList ++ whiteList
     where 
       blackList = Prelude.map (\n -> (L.replicate x True) ++ (False:n) ) $ createCandidates_ (num-x-1) xs (vol-x-1)
       whiteList = Prelude.map (\n -> False:n) $ createCandidates_ (num-1) constraint vol

volume :: [Int] -> Int
volume = sum . (intersperse 1)

match :: [Int] -> [Int] -> [Maybe Int]
match [] _ = []
match _ [] = []
match (x:xs) (y:ys) = let z = if x == y then Just x else Nothing in z : (match xs ys)

solveConstraint :: [(Index,Cell)] -> Constraint -> Maybe ([(Index,Cell)], Constraints)
solveConstraint cells constraint@(xs,(lb,ub)) = if L.null c then Nothing else Just (newCells,newConstraint) 
    where
        targetCells = drop (lb-1) $ take (ub) cells
        targets = Prelude.map snd targetCells
        len = ub - lb + 1
        vol = volume xs
        c = Prelude.filter (adaptLine targets) (createCandidates_ len xs vol)
        candidates = labeling $ head c 
        revCandidates = labeling $ reverse $ head $ Prelude.filter (adaptLine (reverse targets)) (createCandidates_ len (reverse xs) vol)
        line = match candidates revCandidates
        newline = Prelude.map (maybe Nothing (\n -> Just (odd n)) ) line
        newCells = L.foldl' (\cur -> \((i,c),n) -> if isJust n && c /= n  then (i,n):cur else cur) [] (zip targetCells newline)
        l = Prelude.map (\(n,i) -> (div (fromJust n) 2,i)) $ Prelude.filter (maybe False even . fst) $ zip line [lb..ub]
        newConstraint = Prelude.filter (\(n,(lb,ub))-> volume n /= (ub-lb+1)) $ Prelude.filter (not.(L.null).fst) $ createNewConstraint constraint l
        createNewConstraint xs [] = [xs]
        createNewConstraint (xs,(lb,ub)) ((c,i):cs) = let (a,b) = splitAt c xs in (a,(lb,i-1)) : createNewConstraint (b,(i+1,ub)) (Prelude.map (\(n,j) -> (n-c,j)) cs)

createNewLine :: [(Index,Cell)] -> Set Int -> Constraints -> Maybe ([(Index,Cell)], Constraints)
createNewLine lineCell set constraints = 
    let (targets, outOfTargets) = L.partition (\(_,(lb,ub)) -> any (\n -> member n set) [lb..ub]) constraints in 
    let ret = Prelude.map (solveConstraint lineCell) targets in
    if any isNothing ret then Nothing
    else let a = Prelude.map fromJust ret in 
         Just (concat $ Prelude.map fst a, (concat $ Prelude.map snd a) ++ outOfTargets)

