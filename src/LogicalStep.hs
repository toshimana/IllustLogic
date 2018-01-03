module LogicalStep where

import Data.Array.IO 
import Data.Set as T
import Data.Sequence as S
import Data.List as L
import Data.Bool
import Data.Maybe
    
import ILData
import ILFunc
    
newtype MatchLine = MatchLine [Maybe Int]
newtype LabelLine = LabelLine [Int]

newtype ConstraintIndex = ConstraintIndex Int
    
getRangeList :: Range -> [Int]
getRangeList (Range lb ub) = [lb..ub]
                            
splitConstraint :: ConstraintIndex -> Constraint -> (Constraint, Constraint)
splitConstraint (ConstraintIndex i) (Constraint cs) = let (a,b) = L.splitAt i cs in (Constraint a, Constraint b)

divideCandidates :: Int -> Candidates -> (Candidates, Candidates)
divideCandidates n (Candidates cs) =
    let (as, bs) = L.foldr f ([],[]) cs in
    (Candidates as, Candidates bs)
    where
      f (Candidate c) (acur,ecur) =
          let (a,b) = L.splitAt (n-1) c in
          let (d,e) = L.splitAt 1 b in
          ((Candidate a):acur, (Candidate e):ecur)

headCandidates :: Candidates -> Candidate
headCandidates (Candidates cs) = head cs
          
divideRangeConstraint :: RangeConstraint -> [(ConstraintIndex,CellIndex)] -> [RangeConstraint]
divideRangeConstraint (RangeConstraint xs range fc rc) [] =
    let newFrontCandidates = createCandidatesFromCandidate xs (headCandidates fc) in
    let newRearCandidates = createCandidatesRevFromCandidate xs (headCandidates rc) in
    [RangeConstraint xs range newFrontCandidates newRearCandidates]
divideRangeConstraint (RangeConstraint xs (Range lb ub) fc rc) ((ci@(ConstraintIndex c),ii@(CellIndex i)):cs) =
    let (a,b) = splitConstraint ci xs in
    let len = i-lb+1 in
    let (frontCandidates, frontRest) = divideCandidates len fc in
    let (rearCandidates, rearRest) = divideCandidates len rc in
    let newCs = Prelude.map (\(ConstraintIndex n,j) -> (ConstraintIndex (n-c),j)) cs in
    let rest = divideRangeConstraint (RangeConstraint b (Range (i+1) ub) frontRest rearRest) newCs in
    let range = (Range lb (i-1)) in
    let newFrontCandidates = createCandidatesFromCandidate a (headCandidates frontCandidates) in
    let newRearCandidates = createCandidatesRevFromCandidate a (headCandidates rearCandidates) in
    (RangeConstraint a range newFrontCandidates newRearCandidates) : rest

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
createNewConstraints rc@(RangeConstraint constraint bound _ _) (MatchLine matchLine) =
    let whiteCellSeeds = Prelude.filter (maybe False even . fst) $ L.zip matchLine (getRangeList bound) in
    let whiteCellPoints = Prelude.map (\(n,i) -> (convertConstraintIndexFromWhiteCellLabel n, CellIndex i)) whiteCellSeeds in
    let dividedRangeConstraints = divideRangeConstraint rc whiteCellPoints in
    Prelude.filter (\x -> (not (hasConstraint x)) && (not (isCompleted x))) dividedRangeConstraints
    where
      convertConstraintIndexFromWhiteCellLabel (Just n) = ConstraintIndex (div n 2)
      isCompleted (RangeConstraint constraint range _ _) = (volume constraint) == (getLength range)
      hasConstraint (RangeConstraint (Constraint c) _ _ _) = L.null c

nullCandidates :: Candidates -> Bool
nullCandidates (Candidates cs) = L.null cs
                                                             
solveConstraint :: [Cell] -> RangeConstraint -> Maybe ([Cell], Constraints)
solveConstraint cells rc@(RangeConstraint constraint bound frontCandidates rearCandidates) =
   let targetCells = rangeList bound cells in
   let lineStates = BoardLine $ Prelude.map (\(Cell _ ce) -> ce) targetCells in
   let frontValidCandidates = scanCandidates frontCandidates lineStates in
   let rearValidCandidates = scanCandidates rearCandidates lineStates in
   if nullCandidates frontValidCandidates then Nothing
   else
       let matchLine = match (headCandidates frontValidCandidates) (headCandidates rearValidCandidates) in
       let newCells = findChangingCells targetCells matchLine in
       let newRangeConstraint = createNewConstraints (RangeConstraint constraint bound frontValidCandidates rearValidCandidates) matchLine in
       Just (newCells, Constraints newRangeConstraint)

createNewLine :: [Cell] -> CellIndices -> Constraints -> Maybe ([Cell], Constraints)
createNewLine lineCell (CellIndices set) (Constraints constraints) = 
    let (targets, outOfTargets) = L.partition (\(RangeConstraint _ (Range lb ub) _ _) -> any (\n -> member (CellIndex n) set) [lb..ub]) constraints in 
    let ret = Prelude.map (solveConstraint lineCell) targets in
    if any isNothing ret then Nothing
    else let a = Prelude.map fromJust ret in 
         let cs = (concat $ Prelude.map (\(_,Constraints c) -> c) a) ++ outOfTargets in
         Just (concat $ Prelude.map fst a, Constraints cs)

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
