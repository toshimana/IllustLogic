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
    
getRangeList :: Range -> [CellIndex]
getRangeList (Range lb ub) = Prelude.map CellIndex [lb..ub]
                            
splitConstraint :: ConstraintIndex -> Constraint -> (Constraint, Constraint)
splitConstraint (ConstraintIndex i) (Constraint cs) = let (a,b) = L.splitAt i cs in (Constraint a, Constraint b)

divideCandidate :: Int -> Candidate -> (Candidate, Candidate)
divideCandidate n (Candidate c) =
    let (a,b) = L.splitAt (n-1) c in
    let (d,e) = L.splitAt 1 b in
    (Candidate a, Candidate e)
          
divideRangeConstraint :: RangeConstraint -> [(ConstraintIndex,CellIndex)] -> [RangeConstraint]
divideRangeConstraint rc [] = [rc]
divideRangeConstraint (RangeConstraint xs (Range lb ub) fc rc) ((ci@(ConstraintIndex c),ii@(CellIndex i)):cs) =
    let (a,b) = splitConstraint ci xs in
    let len = i-lb+1 in
    let (frontCandidate, frontRest) = divideCandidate len fc in
    let (rearCandidate, rearRest) = divideCandidate len rc in
    let newCs = Prelude.map (\(ConstraintIndex n,j) -> (ConstraintIndex (n-c),j)) cs in
    let rest = divideRangeConstraint (RangeConstraint b (Range (i+1) ub) frontRest rearRest) newCs in
    let range = (Range lb (i-1)) in
    (RangeConstraint a range frontCandidate rearCandidate) : rest

labeling :: Candidate -> LabelLine
labeling (Candidate list) = LabelLine (f list 0)
    where
      f [] cur = []
      f (x:xs) cur = if (odd cur) == x then cur : (f xs cur) else (cur+1):(f xs (cur+1))

match :: LabelLine -> LabelLine -> MatchLine
match (LabelLine xs) (LabelLine ys) = MatchLine (L.zipWith (\x y -> if x == y then Just x else Nothing) xs ys)

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
    let whiteCellPoints = Prelude.map (\(n,i) -> (convertConstraintIndexFromWhiteCellLabel n, i)) whiteCellSeeds in
    let dividedRangeConstraints = divideRangeConstraint rc whiteCellPoints in
    Prelude.filter (\x -> (not (hasConstraint x)) && (not (isCompleted x))) dividedRangeConstraints
    where
      convertConstraintIndexFromWhiteCellLabel (Just n) = ConstraintIndex (div n 2)
      isCompleted (RangeConstraint constraint range _ _) = (volume constraint) == (getLength range)
      hasConstraint (RangeConstraint (Constraint c) _ _ _) = L.null c

findPotentialBlack :: LabelLine -> LabelLine -> [Maybe (Int, Int)]
findPotentialBlack (LabelLine xs) (LabelLine ys) = findPotentialBlackImpl xs ys
    where
      findPotentialBlackImpl [] [] = []
      findPotentialBlackImpl (x:xs) (y:ys) =
          let result = if (odd x) && (odd y) && (x /= y) then Just (x, y) else Nothing in
          result : findPotentialBlackImpl xs ys

replaceList :: Int -> [a] -> a -> [a]
replaceList n xs elt = let (a,b) = L.splitAt (n-1) xs in a ++ (elt:(tail b)) 

matchBlack :: Candidate -> Candidate -> Candidate
matchBlack (Candidate xs) (Candidate ys) = Candidate $ impl xs ys
    where
      impl [] [] = []
      impl (x:xs) (y:ys) = (x && y) : (impl xs ys)
                                   
searchBlack :: Constraint -> BoardLine -> Int -> Maybe Candidate
searchBlack (Constraint constraint) inBlack@(BoardLine bl) con =
    let len = L.length bl in
    let candidates = createCandidates len (Constraint [con]) con in
    let frontCandidates = scanCandidates candidates inBlack in
    let rearCandidates = scanCandidates (reverseEltCandidates candidates) inBlack in
    if (nullCandidates frontCandidates) || (nullCandidates rearCandidates) then Nothing
    else Just $ matchBlack (headCandidates frontCandidates) (headCandidates rearCandidates)
               
mergeBlack :: [CellElt] -> Candidate -> [CellElt]
mergeBlack cur (Candidate xs) = L.zipWith (\(CellElt c) -> \b -> let e = if b then Just True else c in (CellElt e)) cur xs
                                                                
seekPotentialBlack :: Constraint -> [CellElt] -> Maybe (Int,Int) -> CellElt -> Int -> Maybe Candidate
seekPotentialBlack _ _ Nothing _ _ = Nothing
seekPotentialBlack constraint@(Constraint cs) whiteList (Just (n,m)) (CellElt (Just True)) i =
    let len = L.length whiteList in
    let inBlack = BoardLine $ replaceList i whiteList (CellElt (Just True)) in
    let indices = L.map (\a -> div a 2) [m,m+2..n] in
    let con = minimum $ L.map (\i -> cs !! i) indices in
    searchBlack constraint inBlack con
seekPotentialBlack _ _ _ _ _ = Nothing
                   
checkPotentialBlack :: Constraint -> [Maybe (Int, Int)] -> BoardLine -> BoardLine
checkPotentialBlack constraint pbs (BoardLine ml) =
    BoardLine $ L.foldl' mergeBlack ml $ catMaybes $ L.zipWith3 (seekPotentialBlack constraint whiteList) pbs ml [1..]
    where
      whiteList = L.map (\b -> if b == (CellElt (Just True)) then CellElt Nothing else b) ml


nullCandidates :: Candidates -> Bool
nullCandidates (Candidates cs) = L.null cs

mergeNewLine :: [Cell] -> MatchLine -> BoardLine
mergeNewLine cs (MatchLine matchLine) = BoardLine $ impl cs matchLine
    where
      impl [] [] = []
      impl (c@(Cell p e):cs) (m:ms) =
          let n = case m of
                    Nothing -> e
                    Just m -> CellElt (Just (odd m))
          in n : (impl cs ms)

diffLine :: [Cell] -> BoardLine -> [Cell]
diffLine cs (BoardLine bd) = L.foldr impl [] (L.zip cs bd)
    where
      impl :: (Cell,  CellElt) -> [Cell] -> [Cell]
      impl (c@(Cell p (CellElt e1)), b@(CellElt e2)) cur = if (isNothing e1) && (isJust e2) then (Cell p b):cur else cur 

takeWhileCandidates :: (Candidate -> Bool) -> Candidates -> Candidates
takeWhileCandidates pred (Candidates cs) = Candidates $ takeWhile pred cs

lengthCandidate :: Candidate -> Int
lengthCandidate (Candidate c) = L.length c

findPotentialWhite :: LabelLine -> LabelLine -> [Maybe (Int,Int)]
findPotentialWhite (LabelLine xs) (LabelLine ys) = impl xs ys
    where
      impl [] [] = []
      impl (x:xs) (y:ys) =
          let result = if (even x) && (even y) && (x /= y) then Just (x, y) else Nothing in
          result : impl xs ys

mergeWhite :: CellElt -> Bool -> CellElt
mergeWhite c b = if b then CellElt (Just False) else c
                                                                
searchWhite :: Constraint -> BoardLine -> Int -> Bool
searchWhite (Constraint constraint) inBlack@(BoardLine bl) con =
    let len = L.length bl in
    let candidates = createCandidates len (Constraint [con]) con in
    let frontCandidates = scanCandidates candidates inBlack in
    (nullCandidates frontCandidates)
    
seekPotentialWhite :: Constraint -> [CellElt] -> Maybe (Int,Int) -> CellElt -> Int -> Bool
seekPotentialWhite _ _ Nothing _ _ = False
seekPotentialWhite constraint@(Constraint cs) whiteList (Just (n,m)) (CellElt Nothing) i =
    let len = L.length whiteList in
    let inBlack = BoardLine $ replaceList i whiteList (CellElt (Just True)) in
    let indices = L.map (\a -> div a 2) [m,m+2..n-1] in
    let con = minimum $ L.map (\i -> cs !! i) indices in
    searchWhite constraint inBlack con
seekPotentialWhite _ _ _ _ _ = False

checkPotentialWhite :: Constraint -> [Maybe (Int,Int)] -> BoardLine -> BoardLine
checkPotentialWhite constraint pbs (BoardLine ml) =
    BoardLine $ L.zipWith mergeWhite ml $ L.zipWith3 (seekPotentialWhite constraint whiteList) pbs ml [1..]
    where
      whiteList = L.map (\b -> if b == (CellElt (Just True)) then CellElt Nothing else b) ml
                     
solveConstraint :: [Cell] -> RangeConstraint -> Maybe ([Cell], Constraints)
solveConstraint cells rc@(RangeConstraint constraint bound frontCandidate rearCandidate) =
   let targetCells = rangeList bound cells in
   if L.null targetCells then undefined
   else
   let lineStates = BoardLine $ Prelude.map (\(Cell _ ce) -> ce) targetCells in
   let frontValidCandidates = scanCandidates (createCandidatesFromCandidate constraint frontCandidate) lineStates in
   let rearValidCandidates = scanCandidates (createCandidatesRevFromCandidate constraint rearCandidate) lineStates in
   if (nullCandidates frontValidCandidates) || (nullCandidates rearValidCandidates) then Nothing
   else
       let frontValidCandidate = headCandidates frontValidCandidates in
       let rearValidCandidate = headCandidates rearValidCandidates in
       let frontLabelCandidate = labeling frontValidCandidate in
       let rearLabelCandidate = labeling rearValidCandidate in
       let matchLine = match frontLabelCandidate rearLabelCandidate in
       let newLine = mergeNewLine targetCells matchLine in
       let potentialBlack = findPotentialBlack frontLabelCandidate rearLabelCandidate in
       let pbMatchLine = checkPotentialBlack constraint potentialBlack newLine in
       let potentialWhite = findPotentialWhite frontLabelCandidate rearLabelCandidate in
       let pwMatchLine = checkPotentialWhite constraint potentialWhite pbMatchLine in 
       let newCells = diffLine targetCells pwMatchLine in
       let targetRangeConstraint = RangeConstraint constraint bound frontValidCandidate rearValidCandidate in
       let newRangeConstraint = createNewConstraints targetRangeConstraint matchLine in
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
--        if L.null newLines then return () else printArray mb
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
