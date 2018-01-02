module Lib where

import Data.Array.IO 
import Data.Array
import Data.Maybe
import Data.Set as T
import Data.Bool (bool)
import Data.Sequence as S
import Data.List as L (sortBy, groupBy, zipWith, replicate, null, reverse, splitAt, zip, foldl1',foldl',intersperse, minimumBy, delete, partition)

import ILData
import LogicalStep
    

newtype IBoard = IBoard (Array Point CellElt)

newtype IConstraints = IConstraints (Array LineIndex Constraints)

data IProblem = IProblem IBoard IConstraints IConstraints

newtype Depth = Depth Int

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

choiceDirection :: Direction -> a -> a -> a
choiceDirection (Direction d) a b = bool b a d

getCells :: IBoard -> [Cell]
getCells (IBoard ib) = Prelude.map (\(a,b)->Cell a b) (assocs ib)

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
solveIllustLogic rowConstraintRaw colConstraintRaw = do
    let (rlen,clen) = (Prelude.length rowConstraintRaw, Prelude.length colConstraintRaw)
    let rc = Prelude.map (\n -> Constraints [RangeConstraint (Constraint n) (Range (1::Int) clen)]) rowConstraintRaw
    let cc = Prelude.map (\n -> Constraints [RangeConstraint (Constraint n) (Range (1::Int) rlen)]) colConstraintRaw
    rowConstraint <- newListArray (LineIndex 1, LineIndex rlen) rc
    colConstraint <- newListArray (LineIndex 1, LineIndex clen) cc
    mb <- newArray (Point 1 1, Point rlen clen) (CellElt Nothing)
    let allLine = append (createChangeLines True clen rlen) (createChangeLines False rlen clen)
    results <- solve (Depth 0) (SolvingProblem (MProblem (MBoard mb) (MConstraints rowConstraint) (MConstraints colConstraint)) allLine)
    print "[[Result]]"
    Prelude.mapM_ (\(Depth d,b) -> print d >> printArray b) results
    return $ Prelude.map (\(_,b) -> toResult b) results

