module Lib where

import Data.Array.IO 
import Data.Array
import Data.Maybe
import Data.Set as T
import Data.Bool (bool)
import Data.Sequence as S
import Data.List as L

import ILData
import ILFunc
import LogicalStep
import EstimateStep    

newtype Depth = Depth Int

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

createInitRangeConstraints :: Int -> [Int] -> Constraints
createInitRangeConstraints num c =
    Constraints [createRangeConstraint c (Range 1 num)]

solveIllustLogic :: [[Int]] -> [[Int]] -> IO [IOArray (Int,Int) Bool]
solveIllustLogic rowConstraintRaw colConstraintRaw = do
  let (rlen,clen) = (Prelude.length rowConstraintRaw, Prelude.length colConstraintRaw)
  let rc = Prelude.map (\n -> createInitRangeConstraints clen n) rowConstraintRaw
  let cc = Prelude.map (\n -> createInitRangeConstraints rlen n) colConstraintRaw
  rowConstraint <- newListArray (LineIndex 1, LineIndex rlen) rc
  colConstraint <- newListArray (LineIndex 1, LineIndex clen) cc
  mb <- newArray (Point 1 1, Point rlen clen) (CellElt Nothing)
  let allLine = append (createChangeLines True clen rlen) (createChangeLines False rlen clen)
  results <- solve (Depth 0) (SolvingProblem (MProblem (MBoard mb) (MConstraints rowConstraint) (MConstraints colConstraint)) allLine)
  print "[[Result]]"
  Prelude.mapM_ (\(Depth d,b) -> print d >> printArray b) results
  return $ Prelude.map (\(_,b) -> toResult b) results

