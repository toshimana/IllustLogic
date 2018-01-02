module Lib where

import Data.Array.IO 
import Data.Array
import Data.Maybe
import Data.Set as T
import Data.Bool (bool)
import Data.Sequence as S
import Data.List as L (sortBy, groupBy, zipWith, replicate, null, reverse, splitAt, zip, foldl1',foldl',intersperse, minimumBy, delete, partition)

data Range = Range Int Int deriving (Eq, Show)
data Point = Point Int Int deriving (Ix, Ord, Eq, Show)

newtype CellElt = CellElt (Maybe Bool) deriving (Eq, Show)
data Cell = Cell Point CellElt deriving (Eq, Show)

newtype Constraint = Constraint [Int] deriving (Eq, Show)
data RangeConstraint = RangeConstraint Constraint Range deriving (Eq, Show)
newtype Constraints = Constraints [RangeConstraint] deriving (Eq, Show)

newtype MBoard = MBoard (IOArray Point CellElt)
newtype IBoard = IBoard (Array Point CellElt)

newtype ConstraintIndex = ConstraintIndex Int
newtype CellIndex = CellIndex Int deriving (Ord, Eq)
instance Num CellIndex where
  (+) (CellIndex x) (CellIndex y) = CellIndex (x + y)
  (-) (CellIndex x) (CellIndex y) = CellIndex (x - y)
  (*) (CellIndex x) (CellIndex y) = CellIndex (x * y)
  negate (CellIndex x) = CellIndex (negate x)
  abs (CellIndex x) = CellIndex (abs x)
  signum (CellIndex x) = CellIndex (signum x)
  fromInteger a = CellIndex (fromInteger a)

newtype LineIndex = LineIndex Int deriving (Ix, Ord, Eq)
newtype CellIndices = CellIndices (Set CellIndex)

newtype MConstraints = MConstraints (IOArray LineIndex Constraints)
newtype IConstraints = IConstraints (Array LineIndex Constraints)

newtype Candidate = Candidate [Bool] deriving (Show,Eq)
newtype BoardLine = BoardLine [CellElt]
newtype LabelLine = LabelLine [Int]
newtype MatchLine = MatchLine [Maybe Int]

newtype Direction = Direction Bool deriving (Eq)
data Line = Line Direction LineIndex CellIndices
data LinePosition = LinePosition Direction LineIndex CellIndex

data MProblem = MProblem MBoard MConstraints MConstraints
data IProblem = IProblem IBoard IConstraints IConstraints
newtype ChangeLines = ChangeLines (Seq Line)
data SolvingProblem = SolvingProblem MProblem ChangeLines

newtype Depth = Depth Int

adaptLine :: BoardLine -> Candidate -> Bool
adaptLine (BoardLine bline) (Candidate xs) = and $ L.zipWith f bline xs
        where
          f (CellElt (Just a)) b = a==b
          f _ _ = True

labeling :: Candidate -> LabelLine
labeling (Candidate list) = LabelLine (f list 0)
    where
      f [] cur = []
      f (x:xs) cur = if (odd cur) == x then cur : (f xs cur) else (cur+1):(f xs (cur+1))

createCandidatesFromRangeConstraint :: RangeConstraint -> [Candidate]
createCandidatesFromRangeConstraint (RangeConstraint cs (Range lb ub)) = createCandidates (ub-lb+1) cs (volume cs)

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

volume :: Constraint -> Int
volume (Constraint cs) = sum (L.intersperse 1 cs)

cvolume :: Constraint -> Range -> Int
cvolume constraint@(Constraint cs) (Range lb ub) =
    let s = ub - lb + 1 in
    let box = Prelude.length cs + 1 in
    let ball = s - volume constraint in
    f box ball
    where 
        f _ 0 = 1
        f 1 ball = 1
        f box 1 = box
        f box ball = L.foldl' (\cur elt-> cur + f (box-1) (ball-elt)) 0 [0..ball]

getLength :: Range -> Int
getLength (Range lb ub) = ub - lb + 1

match :: Candidate -> Candidate -> MatchLine
match xs ys = 
  let LabelLine lxs = labeling xs in
  let LabelLine lys = labeling ys in
  MatchLine (L.zipWith (\x y -> if x == y then Just x else Nothing) lxs lys)

splitConstraint :: ConstraintIndex -> Constraint -> (Constraint, Constraint)
splitConstraint (ConstraintIndex i) (Constraint cs) = let (a,b) = L.splitAt i cs in (Constraint a, Constraint b)

divideRangeConstraint :: RangeConstraint -> [(ConstraintIndex,CellIndex)] -> [RangeConstraint]
divideRangeConstraint xs [] = [xs]
divideRangeConstraint (RangeConstraint xs (Range lb ub)) ((ci@(ConstraintIndex c),ii@(CellIndex i)):cs) = 
    let (a,b) = splitConstraint ci xs in 
    (RangeConstraint a (Range lb (i-1))) : divideRangeConstraint (RangeConstraint b (Range (i+1) ub)) (Prelude.map (\(ConstraintIndex n,j) -> (ConstraintIndex (n-c),j)) cs)

createFrontCandidate :: RangeConstraint -> BoardLine -> Maybe Candidate
createFrontCandidate (RangeConstraint constraint bound) lineStates =
  listToMaybe validCandidates
  where
    vol = volume constraint
    allCandidates = createCandidates (getLength bound) constraint vol
    validCandidates = Prelude.filter (adaptLine lineStates) allCandidates

reverseCandidate :: Candidate -> Candidate
reverseCandidate (Candidate c) = Candidate (L.reverse c)

reverseConstraint :: Constraint -> Constraint
reverseConstraint (Constraint cs) = Constraint (L.reverse cs)

createRearCandidate :: RangeConstraint -> BoardLine -> Maybe Candidate
createRearCandidate (RangeConstraint constraint bound) lineStates =
  listToMaybe validCandidates
  where
    vol = volume constraint
    allCandidates = createCandidates (getLength bound) (reverseConstraint constraint) vol
    validCandidates = Prelude.filter (adaptLine lineStates) (Prelude.map reverseCandidate allCandidates)

findChangingCells :: [Cell] -> MatchLine -> [Cell]
findChangingCells targetCells (MatchLine matchLine) =
    let newBoardLine = Prelude.map (maybe Nothing (\n -> Just (odd n)) ) matchLine in
    L.foldl' f [] (L.zip targetCells newBoardLine)
    where
      isNew (CellElt old) new = isJust new && old /= new
      f cur (Cell i e, n) = if isNew e n  then (Cell i (CellElt n)):cur else cur

getRangeList :: Range -> [Int]
getRangeList (Range lb ub) = [lb..ub]
                            
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
   if isNothing frontValidCandidate then Nothing else Just (newCells, Constraints newRangeConstraint)
    where
        targetCells = rangeList bound cells
        lineStates = Prelude.map (\(Cell _ ce) -> ce) targetCells
        frontValidCandidate = createFrontCandidate rc (BoardLine lineStates) 
        rearValidCandidate = createRearCandidate rc (BoardLine lineStates)
        matchLine = match (fromJust frontValidCandidate) (fromJust rearValidCandidate)
        newCells = findChangingCells targetCells matchLine
        newRangeConstraint = createNewConstraints rc matchLine

createNewLine :: [Cell] -> CellIndices -> Constraints -> Maybe ([Cell], Constraints)
createNewLine lineCell (CellIndices set) (Constraints constraints) = 
    let (targets, outOfTargets) = L.partition (\(RangeConstraint _ (Range lb ub)) -> any (\n -> member (CellIndex n) set) [lb..ub]) constraints in 
    let ret = Prelude.map (solveConstraint lineCell) targets in
    if any isNothing ret then Nothing
    else let a = Prelude.map fromJust ret in 
         let cs = (concat $ Prelude.map (\(_,Constraints c) -> c) a) ++ outOfTargets in
         Just (concat $ Prelude.map fst a, Constraints cs)

freezeProblem :: MProblem -> IO IProblem
freezeProblem (MProblem (MBoard mb) (MConstraints mrc) (MConstraints mcc)) = do
  ib <- freeze mb
  irc <- freeze mrc
  icc <- freeze mcc
  return (IProblem (IBoard ib) (IConstraints irc) (IConstraints icc))

thawProblem :: IProblem -> IO MProblem
thawProblem (IProblem (IBoard ib) (IConstraints irc) (IConstraints icc)) = do
  mb <- thaw ib
  mrc <- thaw irc
  mcc <- thaw icc
  return (MProblem (MBoard mb) (MConstraints mrc) (MConstraints mcc))

createLineFromBoard :: [Cell] -> Direction -> LineIndex -> [Cell]
createLineFromBoard elements (Direction d) (LineIndex li) =
    Prelude.filter (bool equalColFunc equalRowFunc d) elements
    where
      equalRowFunc (Cell (Point a _) _) = li == a
      equalColFunc (Cell (Point _ a) _) = li == a

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
    

insertCellIndices :: CellIndices -> CellIndex -> CellIndices
insertCellIndices (CellIndices cs) c = CellIndices (insert c cs)

mergeLine :: Line -> CellIndex -> Line
mergeLine (Line lb li ls) ci = Line lb li (insertCellIndices ls ci)

equalLinePosition :: Line -> LinePosition -> Bool
equalLinePosition (Line lb li _) (LinePosition rb ri _) = (lb == rb) && (li == ri)

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

choiceDirection :: Direction -> a -> a -> a
choiceDirection (Direction d) a b = bool b a d

getCells :: IBoard -> [Cell]
getCells (IBoard ib) = Prelude.map (\(a,b)->Cell a b) (assocs ib)

rangeList :: Range -> [a] -> [a]
rangeList (Range lb ub) xs = Prelude.drop (lb-1) $ Prelude.take ub xs

cellElt :: Cell -> CellElt
cellElt (Cell _ ce) = ce

isConfirmed :: CellElt -> Bool
isConfirmed (CellElt ce) = isJust ce

isConfirmedCell :: Cell -> Bool
isConfirmedCell (Cell i ce) = isConfirmed ce

renewCell :: Cell -> Bool -> Cell
renewCell (Cell i _) b = Cell i (CellElt (Just b))

unconfirmedList :: [Cell] -> Candidate -> [Cell]
unconfirmedList bl (Candidate candidate) = 
  L.foldl' (\cur (c,i) -> if isConfirmedCell c then cur else (renewCell c i):cur) [] (L.zip bl candidate)

refineConstraint :: MConstraints -> Bool -> IO [(Direction, LineIndex, RangeConstraint)]
refineConstraint (MConstraints mc) flg = do
  as <- getAssocs mc 
  return $ concatMap (\(i,Constraints cs)-> Prelude.map (\e -> (Direction flg,i,e)) cs) as

getSmallestCostConstraint :: MConstraints -> MConstraints -> IO (Direction, LineIndex, RangeConstraint)
getSmallestCostConstraint mrc mcc = do
  rc <- refineConstraint mrc True
  cc <- refineConstraint mcc False
  return $ minimumBy (\a b -> compare (calcCost a) (calcCost b)) (rc++cc)
  where
    calcCost (_,_,(RangeConstraint cs r)) = cvolume cs r

createRegionCell :: IBoard -> Direction -> LineIndex -> RangeConstraint -> [Cell]
createRegionCell ib di li (RangeConstraint _ r) =
  let cells = getCells ib in rangeList r $ createLineFromBoard cells di li

createEstimateCells :: IBoard -> Direction -> LineIndex -> RangeConstraint -> [[Cell]]
createEstimateCells ib di li constraint =
  let targetCell = createRegionCell ib di li constraint in
  let bl = BoardLine (Prelude.map cellElt targetCell) in
  let newLines = Prelude.filter (adaptLine bl) (createCandidatesFromRangeConstraint constraint) in
  Prelude.map (unconfirmedList targetCell) newLines

rewriteConstraint :: MConstraints -> LineIndex -> RangeConstraint -> IO ()
rewriteConstraint (MConstraints c) li constraint = do
  Constraints cs <- readArray c li
  writeArray c li (Constraints (L.delete constraint cs))

createLineFromLinePosition :: LinePosition -> Line
createLineFromLinePosition (LinePosition bl i e) = Line bl i (CellIndices (T.singleton e))

cloneAndEstimateProblem :: IProblem -> Direction -> [Cell] -> IO SolvingProblem
cloneAndEstimateProblem iproblem direction xs = do
  mp@(MProblem mb _ _) <- thawProblem iproblem
  newLines <- Prelude.mapM (writeCell mb direction) xs
  return (SolvingProblem mp (ChangeLines (L.foldl' (\lines lp -> (createLineFromLinePosition lp) <| lines) S.empty newLines)))

estimateStep :: MProblem -> IO [SolvingProblem]
estimateStep mproblem@(MProblem mb mrc mcc) = do
  (di,li,constraint) <- getSmallestCostConstraint mrc mcc
  rewriteConstraint (choiceDirection di mrc mcc) li constraint
  iproblem@(IProblem ib _ _) <- freezeProblem mproblem
  Prelude.mapM ( cloneAndEstimateProblem iproblem di ) (createEstimateCells ib di li constraint)

isSolved :: MBoard -> IO Bool
isSolved (MBoard mb) = getElems mb >>= return . and . ( Prelude.map (\(CellElt n) -> isJust n) )

incr :: Depth -> Depth
incr (Depth d) = Depth (d+1)

printDepth :: Depth -> IO ()
printDepth (Depth d) = print d

solve :: Depth -> SolvingProblem -> IO [(Depth,MBoard)]
solve depth sp = logicalStep sp >>= maybe (return []) next
    where
      next logicProblem@(MProblem mb _ _) = do
        printDepth depth >> printArray mb
        isSolved mb >>= bool (estimate logicProblem) (return [(depth,mb)])
      estimate logicProblem = 
        estimateStep logicProblem >>= Prelude.mapM (\newp->solve (incr depth) newp) >>= return.concat

printArray :: MBoard -> IO ()
printArray (MBoard mb) = do
  (_,(Point _ clen)) <- getBounds mb
  getElems mb >>= putStr . unlines . f clen . Prelude.map g >> putChar '\n'
      where
        f collen [] = []
        f collen xs = let (a,b) = L.splitAt collen xs in a:f collen b
        g (CellElt Nothing) = '-'
        g (CellElt (Just True)) = 'X'
        g (CellElt (Just False)) = ' '

createChangeLines :: Bool -> Int -> Int -> ChangeLines
createChangeLines d w n = ChangeLines (createChangeLines_ d w n)
  where
    createChangeLines_ d w 0 = S.empty
    createChangeLines_ d w num = (createChangeLines_ d w (num-1)) |> newline
      where 
        newline = Line (Direction d) (LineIndex num) newCellIndices
        newCellIndices = CellIndices (T.fromList allCells)
        allCells = Prelude.map (\n->CellIndex n) [1..w]

toResult :: MBoard -> IOArray (Int,Int) Bool
toResult (MBoard mb) = undefined

append :: ChangeLines -> ChangeLines -> ChangeLines
append (ChangeLines l) (ChangeLines r) = ChangeLines (l >< r)

solveIllustLogic :: [[Int]] -> [[Int]] -> IO [IOArray (Int,Int) Bool]
solveIllustLogic rowConstraint colConstraint = do
    let (rlen,clen) = (Prelude.length rowConstraint, Prelude.length colConstraint)
    let rc = Prelude.map (\n -> Constraints [RangeConstraint (Constraint n) (Range (1::Int) clen)]) rowConstraint
    let cc = Prelude.map (\n -> Constraints [RangeConstraint (Constraint n) (Range (1::Int) rlen)]) colConstraint
    rowConstraint <- newListArray (LineIndex 1, LineIndex rlen) rc
    colConstraint <- newListArray (LineIndex 1, LineIndex clen) cc
    mb <- newArray (Point 1 1, Point rlen clen) (CellElt Nothing)
    let allLine = append (createChangeLines True clen rlen) (createChangeLines False rlen clen)
    results <- solve (Depth 0) (SolvingProblem (MProblem (MBoard mb) (MConstraints rowConstraint) (MConstraints colConstraint)) allLine)
    print "[[Result]]"
    Prelude.mapM_ (\(Depth d,b) -> print d >> printArray b) results
    return $ Prelude.map (\(_,b) -> toResult b) results

