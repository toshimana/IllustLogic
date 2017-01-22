import Lib

import Data.Array.IO 
import Data.Array
import Data.Maybe
import Data.Either
import Data.Bits (complement, (.&.), (.|.), shiftL)
import Data.Sequence as S
import Data.List as L (sortBy, groupBy, zipWith, replicate, null, reverse, splitAt, zip, foldl1',intersperse)

type Cell = Maybe Bool
type Index = (Int, Int)
type MBoard = IOArray Index Cell
type IBoard = Array Index Cell

type Constraints = [Constraint]
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

toBoolList :: Int -> Candidate -> [Bool]
toBoolList 0 _ = []
toBoolList num c = let (d,m) = divMod c 2 in (m==1):(toBoolList (num-1) d)

toCandidate :: [Bool] -> Candidate
toCandidate [] = 0
toCandidate (x:xs) = if x then 1 + (shiftL (toCandidate xs) 1) else (shiftL (toCandidate xs) 1)
                                    
adaptLine_ :: [Cell] -> [Bool] -> Bool
adaptLine_ line xs = 
    and $ L.zipWith f line xs
        where
          f (Just True) False = False
          f (Just False) True = False
          f _ _ = True

adaptLine :: (Candidate,Candidate) -> Candidate -> Bool
adaptLine p@(ts,fs) xs =
    (ts == (ts .&. xs)) && (fs == (fs .|. xs) )

              
adaptLines :: [Cell] -> [Candidate] -> [Candidate]
adaptLines line xs =
    let p = (f line, g line) in
    Prelude.filter (adaptLine p) xs
    where
      f [] = 0
      f ((Just True):xs) = 1+(shiftL (f xs) 1)
      f (_:xs) = shiftL (f xs) 1
      g [] = 0
      g ((Just False):xs) = shiftL (g xs) 1
      g (_:xs) = 1+(shiftL (g xs) 1)
              
labeling :: [Bool] -> [Int]
labeling list = f list 0 
    where
      f [] cur = []
      f (x:xs) cur = if (odd cur) == x then cur : (f xs cur) else (cur+1):(f xs (cur+1))

equalRow ((a,_),_) ((b,_),_) = a==b

rewriteLine :: [((Index,Cell),Cell)] -> [(Index,Cell)]
rewriteLine [] = []
rewriteLine (x:xs) =
    let (cell,new) = x in
    if (isNothing (snd cell)) && (isJust new) then (fst cell,new):(rewriteLine xs) 
    else rewriteLine xs

logicalLineStep :: [(Index,Cell)] -> [Candidate] -> Maybe [(Index,Cell)]
logicalLineStep cells candidates = 
    if (L.null candidates) then Nothing
    else let headNums = labeling $ toBoolList len $ head candidates in
         let tailNums = labeling $ toBoolList len $ last candidates in
         let newLine = L.zipWith match headNums tailNums in
         Just (rewriteLine $ L.zip cells newLine)
    where
      len = Prelude.length cells
      match a b = 
          if a == b then Just ( odd a )
          else Nothing

createLineFromBoard :: [(Index,Cell)] -> Bool -> Int -> [(Index,Cell)]
createLineFromBoard elements direction index =
    Prelude.filter equalFunc elements
    where
      equalFunc = if direction then equalRowFunc else equalColFunc
      equalRowFunc ((a,_),_) = index == a
      equalColFunc ((_,a),_) = index == a

createNewLine :: MConstraints -> Int -> [(Index,Cell)] -> IO (Maybe [(Index,Cell)])
createNewLine constraints num lineCell =
    let len = Prelude.length lineCell in
    let match a b = if a == b then Just (odd a) else Nothing in
    let currentCells = map snd lineCell in
    do
      constraints_ <- readArray constraints num
      let constraint = head constraints_
      let can = createCandidates constraint
      let c = Prelude.filter (\n -> adaptLine_ currentCells n) can
      let headLine = labeling $ head c
      let revConstraint = (L.reverse $ fst constraint, snd constraint)
      let cc = Prelude.filter (\n -> adaptLine_ (L.reverse currentCells) n) (createCandidates revConstraint)
      let tailLine = labeling $ L.reverse $ head cc
      if L.null c then return Nothing
      else let line = L.zipWith match headLine tailLine in
           return $ Just $ rewriteLine (L.zip lineCell line)
             
                               
logicalLinesStep :: MProblem -> Line -> IO (Maybe (MProblem, [Line]))
logicalLinesStep problem@(mb,rc,cc) line@(direction,num) = do
    elems <- getAssocs mb
    let lineCell = createLineFromBoard elems direction num
    newCells <- createNewLine constraint num lineCell
    case newCells of
      Nothing -> return Nothing
      Just rewriteCells -> do
        newLines <- mapM (writeCell mb) rewriteCells
--        if L.null newLines then return () else printArray mb
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
convertCandidate constraint line = map toCandidate $ Prelude.filter (adaptLine_ line) $ createCandidates constraint

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

