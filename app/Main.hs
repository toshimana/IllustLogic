import Lib

import Data.Array.IO 
import Data.Array
import Data.Maybe
import Data.Either
import Data.Bits (complement, (.&.), (.|.), shiftL)
import Data.Sequence as S
import Data.List as L (sortBy, groupBy, zipWith, replicate, null, reverse, splitAt, zip, foldl1',intersperse)

type MBoard = IOArray Index Cell
type IBoard = Array Index Cell

type MConstraints = IOArray Int Constraints
type IConstraints = Array Int Constraints

type Line = (Bool,Int) -- (direction::Bool, Number::Int)

type Candidate = Integer
type Candidates = [Candidate]

type MProblem = (MBoard, MConstraints, MConstraints)
type IProblem = (IBoard, IConstraints, IConstraints)
    
inputdata :: [String] -> ([Constraints],[Constraints])
inputdata cs = 
    (map (\n -> [(n,(1,Prelude.length cc))]) rc, map (\n -> [(n, (1,Prelude.length rc))]) cc)
    where
      rc = map g rowStr
      cc = map g colStr
      g = map read . words
      (rowStr,colStr) = let (a,b) = break (\s -> s == "") cs in (a,tail b)

freezeProblem :: MProblem -> IO IProblem
freezeProblem (mb,mrc,mcc) = do
  ib <- freeze mb
  irc <- freeze mrc
  icc <- freeze mcc
  return (ib,irc,icc)

thawProblem :: IProblem -> IO MProblem
thawProblem (ib,irc,icc) = do
  mb <- thaw ib
  mrc <- thaw irc
  mcc <- thaw icc
  return (mb,mrc,mcc)

toCandidate :: [Bool] -> Candidate
toCandidate [] = 0
toCandidate (x:xs) = if x then 1 + (shiftL (toCandidate xs) 1) else (shiftL (toCandidate xs) 1)
                                    
createLineFromBoard :: [(Index,Cell)] -> Bool -> Int -> [(Index,Cell)]
createLineFromBoard elements direction index =
    Prelude.filter equalFunc elements
    where
      equalFunc = if direction then equalRowFunc else equalColFunc
      equalRowFunc ((a,_),_) = index == a
      equalColFunc ((_,a),_) = index == a


createNewLine_ :: MConstraints -> Int -> [(Index,Cell)] -> IO (Maybe [(Index,Cell)])             
createNewLine_ mc num lineCell = do
    constraints <- readArray mc num
    let newLine = createNewLine lineCell constraints
--    print (constraints,lineCell,newLine)
    case newLine of
        Nothing -> return Nothing
        Just (newCells, newConstraints) -> do
            writeArray mc num newConstraints
            return (Just newCells)

logicalLinesStep :: MProblem -> Line -> IO (Maybe (MProblem, [Line]))
logicalLinesStep problem@(mb,rc,cc) line@(direction,num) = do
    elems <- getAssocs mb
    let lineCell = createLineFromBoard elems direction num
    newCells <- createNewLine_ constraint num lineCell
    case newCells of
      Nothing -> return Nothing
      Just rewriteCells -> do
        newLines <- mapM (writeCell mb) rewriteCells
        if L.null newLines then return () else printArray mb
        return (Just (problem, newLines))
     where
       constraint = if direction then rc else cc
       writeCell board (index,cell) = do
         writeArray board index cell
         return $ if direction then (False,snd index) else (True,fst index)
    
                               
logicalStep :: MProblem -> Seq Line -> IO (Maybe MProblem)
logicalStep problem@(mb,rc,cc) seql =
    case viewl seql of 
      EmptyL -> return (Just problem)
      e :< es -> do
        ret <- logicalLinesStep problem e
        case ret of
          Nothing -> return Nothing
          Just (newProblem,changeLines) ->
                let newLines = insertLine es changeLines in
                logicalStep newProblem newLines
                    where
                      insertLine ls [] = ls
                      insertLine ls (x:xs) = if elem x ls then insertLine ls xs else insertLine (ls |> x) xs
  
convertCandidate :: Constraint -> [Cell] -> [Candidate]
convertCandidate constraint line = map toCandidate $ Prelude.filter (adaptLine line) $ createCandidates constraint

estimateStep :: MProblem -> IO [(MProblem,Seq Line)]
estimateStep mproblem = undefined

isSolved :: MBoard -> IO Bool
isSolved mb = getElems mb >>= return . and . ( map (\n -> isJust n) )

solve :: Int -> MProblem -> Seq Line -> IO [(Int,MBoard)]
solve depth problem seql= do
    returnLogic <- logicalStep problem seql
    case returnLogic of
      Nothing -> return []
      Just logicProblem@(mb,_,_) -> 
          do
            print depth
            printArray mb
            solved <- isSolved mb
            if solved then return [(depth,mb)]
            else do
              estimateStep logicProblem >>= mapM (\(p,l)->solve (depth+1) p l) >>= return.concat

printArray :: MBoard -> IO ()
printArray mb = do
  (_,(_,clen)) <- getBounds mb
  es <- getElems mb
  putStr $ unlines $ f clen  $ map g es
  putChar '\n'
      where
        f collen [] = []
        f collen xs = let (a,b) = L.splitAt collen xs in a:f collen b
        g Nothing = '-'
        g (Just True) = 'X'
        g (Just False) = ' '

createLineSeq :: Bool -> Int -> Seq Line
createLineSeq direction 0 = empty
createLineSeq direction num = (createLineSeq direction (num-1)) |> (direction, num)
    
main = do
    cs <- getContents
    let (rc, cc) = inputdata $ lines cs
    if let f = sum.concat.(map (concat.(map fst))) in f rc /= f cc then print "Input Data Failed."
    else do
        rowConstraint <- newListArray (1,Prelude.length rc) rc
        colConstraint <- newListArray (1,Prelude.length cc) cc
        mb <- newArray ((1,1),(Prelude.length rc, Prelude.length cc)) Nothing
        let allLine = (createLineSeq True (Prelude.length rc)) >< (createLineSeq False (Prelude.length cc))
        results <- solve 0 (mb, rowConstraint, colConstraint) allLine
        print "[[Result]]"
        mapM_ (\(d,b) -> do
            print d
            printArray b) results

