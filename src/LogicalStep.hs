module LogicalStep where

import Data.Array.IO 
import Data.Set as T
import Data.Sequence as S
import Data.List as L
import Data.Bool
import Data.Maybe
    
import ILData

newtype BoardLine = BoardLine [CellElt]
newtype Candidate = Candidate [Bool] deriving (Show,Eq)
newtype MatchLine = MatchLine [Maybe Int]
newtype LabelLine = LabelLine [Int]

newtype ConstraintIndex = ConstraintIndex Int
    
rangeList :: Range -> [a] -> [a]
rangeList (Range lb ub) xs = Prelude.drop (lb-1) $ Prelude.take ub xs

getLength :: Range -> Int
getLength (Range lb ub) = ub - lb + 1

getRangeList :: Range -> [Int]
getRangeList (Range lb ub) = [lb..ub]
                            
reverseConstraint :: Constraint -> Constraint
reverseConstraint (Constraint cs) = Constraint (L.reverse cs)

reverseCandidate :: Candidate -> Candidate
reverseCandidate (Candidate c) = Candidate (L.reverse c)

volume :: Constraint -> Int
volume (Constraint cs) = sum (L.intersperse 1 cs)

adaptLine :: BoardLine -> Candidate -> Bool
adaptLine (BoardLine bline) (Candidate xs) = and $ L.zipWith f bline xs
        where
          f (CellElt (Just a)) b = a==b
          f _ _ = True

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

createFrontCandidate :: RangeConstraint -> BoardLine -> Maybe Candidate
createFrontCandidate (RangeConstraint constraint bound) lineStates =
  listToMaybe validCandidates
  where
    vol = volume constraint
    allCandidates = createCandidates (getLength bound) constraint vol
    validCandidates = Prelude.filter (adaptLine lineStates) allCandidates

createRearCandidate :: RangeConstraint -> BoardLine -> Maybe Candidate
createRearCandidate (RangeConstraint constraint bound) lineStates =
  listToMaybe validCandidates
  where
    vol = volume constraint
    allCandidates = createCandidates (getLength bound) (reverseConstraint constraint) vol
    validCandidates = Prelude.filter (adaptLine lineStates) (Prelude.map reverseCandidate allCandidates)

splitConstraint :: ConstraintIndex -> Constraint -> (Constraint, Constraint)
splitConstraint (ConstraintIndex i) (Constraint cs) = let (a,b) = L.splitAt i cs in (Constraint a, Constraint b)

divideRangeConstraint :: RangeConstraint -> [(ConstraintIndex,CellIndex)] -> [RangeConstraint]
divideRangeConstraint xs [] = [xs]
divideRangeConstraint (RangeConstraint xs (Range lb ub)) ((ci@(ConstraintIndex c),ii@(CellIndex i)):cs) = 
    let (a,b) = splitConstraint ci xs in 
    (RangeConstraint a (Range lb (i-1))) : divideRangeConstraint (RangeConstraint b (Range (i+1) ub)) (Prelude.map (\(ConstraintIndex n,j) -> (ConstraintIndex (n-c),j)) cs)

labeling :: Candidate -> LabelLine
labeling (Candidate list) = LabelLine (f list 0)
    where
      f [] cur = []
      f (x:xs) cur = if (odd cur) == x then cur : (f xs cur) else (cur+1):(f xs (cur+1))

match :: Candidate -> Candidate -> MatchLine
match xs ys = 
  let LabelLine lxs = labeling xs in
  let LabelLine lys = labeling ys in
  MatchLine (L.zipWith (\x y -> if x == y then Just x else Nothing) lxs lys)

findChangingCells :: [Cell] -> MatchLine -> [Cell]
findChangingCells targetCells (MatchLine matchLine) =
    let newBoardLine = Prelude.map (maybe Nothing (\n -> Just (odd n)) ) matchLine in
    L.foldl' f [] (L.zip targetCells newBoardLine)
    where
      isNew (CellElt old) new = isJust new && old /= new
      f cur (Cell i e, n) = if isNew e n  then (Cell i (CellElt n)):cur else cur

createNewConstraints :: RangeConstraint -> MatchLine -> [RangeConstraint]
createNewConstraints rc@(RangeConstraint constraint bound) (MatchLine matchLine) =
    let whiteCellSeeds = Prelude.filter (maybe False even . fst) $ L.zip matchLine (getRangeList bound) in
    let whiteCellPoints = Prelude.map (\(n,i) -> (convertConstraintIndexFromWhiteCellLabel n, CellIndex i)) whiteCellSeeds in
    let dividedRangeConstraints = divideRangeConstraint rc whiteCellPoints in
    Prelude.filter (\x -> (not (hasConstraint x)) && (not (isCompleted x))) dividedRangeConstraints
    where
      convertConstraintIndexFromWhiteCellLabel (Just n) = ConstraintIndex (div n 2)
      isCompleted (RangeConstraint constraint range) = (volume constraint) == (getLength range)
      hasConstraint (RangeConstraint (Constraint c) _) = L.null c
                            
solveConstraint :: [Cell] -> RangeConstraint -> Maybe ([Cell], Constraints)
solveConstraint cells rc@(RangeConstraint constraint@(Constraint cs) bound@(Range lb ub)) =
   let targetCells = rangeList bound cells in
   let lineStates = Prelude.map (\(Cell _ ce) -> ce) targetCells in
   let frontValidCandidate = createFrontCandidate rc (BoardLine lineStates) in
   let rearValidCandidate = createRearCandidate rc (BoardLine lineStates) in
   if isNothing frontValidCandidate then Nothing
   else
       let matchLine = match (fromJust frontValidCandidate) (fromJust rearValidCandidate) in
       let newCells = findChangingCells targetCells matchLine in
       let newRangeConstraint = createNewConstraints rc matchLine in
       Just (newCells, Constraints newRangeConstraint)                             

createNewLine :: [Cell] -> CellIndices -> Constraints -> Maybe ([Cell], Constraints)
createNewLine lineCell (CellIndices set) (Constraints constraints) = 
    let (targets, outOfTargets) = L.partition (\(RangeConstraint _ (Range lb ub)) -> any (\n -> member (CellIndex n) set) [lb..ub]) constraints in 
    let ret = Prelude.map (solveConstraint lineCell) targets in
    if any isNothing ret then Nothing
    else let a = Prelude.map fromJust ret in 
         let cs = (concat $ Prelude.map (\(_,Constraints c) -> c) a) ++ outOfTargets in
         Just (concat $ Prelude.map fst a, Constraints cs)

createLineFromBoard :: [Cell] -> Direction -> LineIndex -> [Cell]
createLineFromBoard elements (Direction d) (LineIndex li) =
    Prelude.filter (bool equalColFunc equalRowFunc d) elements
    where
      equalRowFunc (Cell (Point a _) _) = li == a
      equalColFunc (Cell (Point _ a) _) = li == a

logicalLinesStep :: MProblem -> Line -> IO (Maybe (MProblem, [LinePosition]))
logicalLinesStep problem@(MProblem mb@(MBoard board) mrc mcc) (Line direction num set) = do
    elems <- getAssocs board
    let cells = Prelude.map (\(a,b) -> Cell a b) elems
    let lineCell = createLineFromBoard cells direction num
    createNewLine_ (choiceConstraint direction) num set lineCell >>= maybe (return Nothing) writeCells
     where
      choiceConstraint (Direction d) = bool mcc mrc d
      writeCells rewriteCells = do
        newLines <- Prelude.mapM (writeCell mb direction) rewriteCells
--         if L.null newLines then return () else printArray mb
        return (Just (problem, newLines))

createNewLine_ :: MConstraints -> LineIndex -> CellIndices -> [Cell] -> IO (Maybe [Cell])
createNewLine_ (MConstraints mc) li set lineCell = do
    constraints <- readArray mc li
    maybe (return Nothing) (rewriteNewCell mc li) (createNewLine lineCell set constraints)
      where
        rewriteNewCell mc li (newCells, newConstraints) = do
          writeArray mc li newConstraints
          return (Just newCells)

writeCell :: MBoard -> Direction -> Cell -> IO LinePosition
writeCell (MBoard board) (Direction d) (Cell index@(Point ridx cidx) cell) = do
    writeArray board index cell
    return $ if d then LinePosition (Direction False) (LineIndex cidx) (CellIndex ridx) else LinePosition (Direction True) (LineIndex ridx) (CellIndex cidx)
               
equalLinePosition :: Line -> LinePosition -> Bool
equalLinePosition (Line lb li _) (LinePosition rb ri _) = (lb == rb) && (li == ri)

mergeLine :: Line -> CellIndex -> Line
mergeLine (Line lb li ls) ci = Line lb li (insertCellIndices ls ci)

insertCellIndices :: CellIndices -> CellIndex -> CellIndices
insertCellIndices (CellIndices cs) c = CellIndices (T.insert c cs)

logicalStep :: SolvingProblem -> IO (Maybe MProblem)
logicalStep (SolvingProblem problem (ChangeLines seql)) =
    case viewl seql of 
      EmptyL -> return (Just problem)
      e :< es -> logicalLinesStep problem e >>= maybe (return Nothing) (nextLogicalStep (ChangeLines es))
      where
        insertLine :: ChangeLines -> LinePosition -> ChangeLines
        insertLine (ChangeLines ls) x = ChangeLines (insertLine_ ls x)
        insertLine_ ls x@(LinePosition xb xi xe) = case viewl ls of
            EmptyL -> S.singleton (Line xb xi (CellIndices (T.singleton xe)))
            e :< ess -> if equalLinePosition e x then (mergeLine e xe) <| ess else e <| (insertLine_ ess x)
        nextLogicalStep :: ChangeLines -> (MProblem, [LinePosition]) -> IO (Maybe MProblem)
        nextLogicalStep es (newProblem,changeLines) = let newLines = L.foldl' insertLine es changeLines in logicalStep (SolvingProblem newProblem newLines)
