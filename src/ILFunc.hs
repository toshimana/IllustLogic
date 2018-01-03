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

getLength :: Range -> Int
getLength (Range lb ub) = ub - lb + 1

reverseConstraint :: Constraint -> Constraint
reverseConstraint (Constraint cs) = Constraint (L.reverse cs)

reverseCandidate :: Candidate -> Candidate
reverseCandidate (Candidate c) = Candidate (L.reverse c)

reverseEltCandidates :: Candidates -> Candidates
reverseEltCandidates (Candidates cs) = Candidates $ Prelude.map reverseCandidate cs
                                 
scanCandidates :: Candidates -> BoardLine -> Candidates
scanCandidates (Candidates candidates) lineStates =
    Candidates (Prelude.dropWhile (not . (adaptLine lineStates)) candidates)

rangeConstraint :: [Int] -> (Int,Int) -> [[Bool]] -> [[Bool]] -> RangeConstraint
rangeConstraint constraint (r1,r2) fc rc = RangeConstraint (Constraint constraint) (Range r1 r2) (Candidates $ Prelude.map Candidate fc) (Candidates $ Prelude.map Candidate rc)

createLineFromBoard :: [Cell] -> Direction -> LineIndex -> [Cell]
createLineFromBoard elements (Direction d) (LineIndex li) =
    Prelude.filter (bool equalColFunc equalRowFunc d) elements
    where
      equalRowFunc (Cell (Point a _) _) = li == a
      equalColFunc (Cell (Point _ a) _) = li == a

createCandidatesImpl :: Int -> Constraint -> Int -> [[Bool]]
createCandidatesImpl num (Constraint []) _ = [L.replicate num False]
createCandidatesImpl num constraint@(Constraint (x:xs)) vol =
    let blackList = Prelude.map (\n -> (L.replicate x True) ++ (False:n) ) $ createCandidatesImpl (num-x-1) (Constraint xs) (vol-x-1) in
    let whiteList = Prelude.map (\n -> False:n) $ createCandidatesImpl (num-1) constraint vol in
    case compare num vol of
      LT -> []
      EQ -> if L.null xs then [L.replicate x True] else blackList
      GT -> blackList ++ whiteList

createCandidates :: Int -> Constraint -> Int -> Candidates
createCandidates num constraint vol = Candidates (Prelude.map Candidate (createCandidatesImpl num constraint vol))

createCandidatesFromCandidate :: Constraint -> Candidate -> Candidates
createCandidatesFromCandidate (Constraint constraint) (Candidate candidate) =
    let len = L.length candidate in
    Candidates $ Prelude.map Candidate $ impl len constraint candidate
    where
      impl :: Int -> [Int] -> [Bool] -> [[Bool]]
      impl len [] c = [L.replicate len False]
      impl len con@(x:xs) c =
          let (a,b) = span not c in
          let wlen = L.length a in
          let blackList = let (bs,rest) = span id b in let blen = L.length bs in Prelude.map (\n -> a ++ bs ++ n) $ impl (len-wlen-blen) xs rest in
          let vol = volume (Constraint con) in
          let whiteList = Prelude.map (\n -> a++False:n) (createCandidatesImpl (len-wlen-1) (Constraint con) vol) in
          case compare (len-wlen) vol of
            LT -> []
            EQ -> if L.null xs then [L.replicate x True] else blackList
            GT -> blackList ++ whiteList

createCandidatesRevFromCandidate :: Constraint -> Candidate -> Candidates
createCandidatesRevFromCandidate constraint candidate =
    reverseEltCandidates $ createCandidatesFromCandidate (reverseConstraint constraint) (reverseCandidate candidate)                  
                                                                               
writeCell :: MBoard -> Direction -> Cell -> IO LinePosition
writeCell (MBoard board) (Direction d) (Cell index@(Point ridx cidx) cell) = do
    writeArray board index cell
    return $ if d then LinePosition (Direction False) (LineIndex cidx) (CellIndex ridx) else LinePosition (Direction True) (LineIndex ridx) (CellIndex cidx)
               
createRangeConstraint :: [Int] -> Range -> RangeConstraint
createRangeConstraint c range =
    let constraint = Constraint c in
    let len = getLength range in
    let vol = volume constraint in
    let frontCandidates = createCandidates len constraint vol in
    let rearCandidates = reverseEltCandidates $ createCandidates len (reverseConstraint constraint) vol in
    RangeConstraint constraint range frontCandidates rearCandidates
