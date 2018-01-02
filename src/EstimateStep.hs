module EstimateStep where

import Data.Array
import Data.Array.IO
import Data.Bool
import Data.Maybe
import Data.Set as T
import Data.List as L
import Data.Sequence as S
    
import ILData
import ILFunc
    
newtype IBoard = IBoard (Array Point CellElt)
newtype IConstraints = IConstraints (Array LineIndex Constraints)
data IProblem = IProblem IBoard IConstraints IConstraints

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

getCells :: IBoard -> [Cell]
getCells (IBoard ib) = Prelude.map (\(a,b)->Cell a b) (assocs ib)

isConfirmed :: CellElt -> Bool
isConfirmed (CellElt ce) = isJust ce

isConfirmedCell :: Cell -> Bool
isConfirmedCell (Cell i ce) = isConfirmed ce

choiceDirection :: Direction -> a -> a -> a
choiceDirection (Direction d) a b = bool b a d

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

createLineFromLinePosition :: LinePosition -> Line
createLineFromLinePosition (LinePosition bl i e) = Line bl i (CellIndices (T.singleton e))

renewCell :: Cell -> Bool -> Cell
renewCell (Cell i _) b = Cell i (CellElt (Just b))

createRegionCell :: IBoard -> Direction -> LineIndex -> RangeConstraint -> [Cell]
createRegionCell ib di li (RangeConstraint _ r) =
  let cells = getCells ib in rangeList r $ createLineFromBoard cells di li

createEstimateCells :: IBoard -> Direction -> LineIndex -> RangeConstraint -> [[Cell]]
createEstimateCells ib di li constraint =
  let targetCell = createRegionCell ib di li constraint in
  let bl = BoardLine (Prelude.map cellElt targetCell) in
  let newLines = Prelude.filter (adaptLine bl) (createCandidatesFromRangeConstraint constraint) in
  Prelude.map (unconfirmedList targetCell) newLines

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

rewriteConstraint :: MConstraints -> LineIndex -> RangeConstraint -> IO ()
rewriteConstraint (MConstraints c) li constraint = do
  Constraints cs <- readArray c li
  writeArray c li (Constraints (L.delete constraint cs))

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
