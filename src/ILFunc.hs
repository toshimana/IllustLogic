module ILFunc where

import Data.Array.IO
import Data.Bool
import Data.List as L
    
import ILData

cellElt :: Cell -> CellElt
cellElt (Cell _ ce) = ce

volume :: Constraint -> Int
volume (Constraint cs) = sum (L.intersperse 1 cs)

rangeList :: Range -> [a] -> [a]
rangeList (Range lb ub) xs = Prelude.drop (lb-1) $ Prelude.take ub xs

adaptLine :: BoardLine -> Candidate -> Bool
adaptLine (BoardLine bline) (Candidate xs) = and $ L.zipWith f bline xs
        where
          f (CellElt (Just a)) b = a==b
          f _ _ = True

rangeConstraint :: [Int] -> (Int,Int) -> RangeConstraint
rangeConstraint constraint (r1,r2) = RangeConstraint (Constraint constraint) (Range r1 r2)

createLineFromBoard :: [Cell] -> Direction -> LineIndex -> [Cell]
createLineFromBoard elements (Direction d) (LineIndex li) =
    Prelude.filter (bool equalColFunc equalRowFunc d) elements
    where
      equalRowFunc (Cell (Point a _) _) = li == a
      equalColFunc (Cell (Point _ a) _) = li == a

createCandidates :: Int -> Constraint -> Int -> [Candidate]
createCandidates num constraint vol = Prelude.map Candidate (createCandidates_ num constraint vol)
      where
        createCandidates_ :: Int -> Constraint -> Int -> [[Bool]]
        createCandidates_ num (Constraint []) _ = [L.replicate num False]
        createCandidates_ num constraint@(Constraint (x:xs)) vol =
          let blackList = Prelude.map (\n -> (L.replicate x True) ++ (False:n) ) $ createCandidates_ (num-x-1) (Constraint xs) (vol-x-1) in
          let whiteList = Prelude.map (\n -> False:n) $ createCandidates_ (num-1) constraint vol in
          case compare num vol of
            LT -> []
            EQ -> if L.null xs then [L.replicate x True] else blackList
            GT -> blackList ++ whiteList

createCandidatesFromRangeConstraint :: RangeConstraint -> [Candidate]
createCandidatesFromRangeConstraint (RangeConstraint cs (Range lb ub)) = createCandidates (ub-lb+1) cs (volume cs)
 
writeCell :: MBoard -> Direction -> Cell -> IO LinePosition
writeCell (MBoard board) (Direction d) (Cell index@(Point ridx cidx) cell) = do
    writeArray board index cell
    return $ if d then LinePosition (Direction False) (LineIndex cidx) (CellIndex ridx) else LinePosition (Direction True) (LineIndex ridx) (CellIndex cidx)
               
