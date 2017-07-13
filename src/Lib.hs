module Lib ( solveIllustLogic ) where

import Data.Array.IO 
import Data.Array
import Data.Maybe
import Data.Set as T
import Data.Bool (bool)
import Data.Sequence as S
import Data.List as L (sortBy, groupBy, zipWith, replicate, null, reverse, splitAt, zip, foldl1',foldl',intersperse, minimumBy, delete, partition)

data Range = Range Int Int deriving (Eq)
data Point = Point Int Int deriving (Ix, Ord, Eq)

newtype CellIndices = CellIndices (Set Int)

newtype CellElt = CellElt (Maybe Bool)
data Cell = Cell Point CellElt

newtype Constraint = Constraint [Int] deriving (Eq)
data RangeConstraint = RangeConstraint Constraint Range deriving (Eq)
newtype Constraints = Constraints [RangeConstraint]

newtype MBoard = MBoard (IOArray Point CellElt)
newtype IBoard = IBoard (Array Point CellElt)

newtype CellIndex = CellIndex Int
newtype LineIndex = LineIndex Int deriving (Ix, Ord, Eq)

newtype MConstraints = MConstraints (IOArray LineIndex Constraints)
newtype IConstraints = IConstraints (Array LineIndex Constraints)

newtype Candidate = Candidate [Bool]
newtype BoardLine = BoardLine [CellElt]
newtype LabelLine = LabelLine [Int]
newtype MatchLine = MatchLine [Maybe Int]

newtype Direction = Direction Bool deriving (Eq)
data Line = Line Direction LineIndex CellIndices

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

createCandidates :: RangeConstraint -> [Candidate]
createCandidates (RangeConstraint cs (Range lb ub)) = createCandidates_ (ub-lb+1) cs (volume cs)


createCandidates_ :: Int -> Constraint -> Int -> [Candidate]
createCandidates_ num constraint vol = Prelude.map Candidate (createCandidates__ num constraint vol)
      where
        createCandidates__ :: Int -> Constraint -> Int -> [[Bool]]
        createCandidates__ num (Constraint []) _ = [L.replicate num False]
        createCandidates__ num constraint@(Constraint (x:xs)) vol =
          let blackList = Prelude.map (\n -> (L.replicate x True) ++ (False:n) ) $ createCandidates__ (num-x-1) (Constraint xs) (vol-x-1) in
          let whiteList = Prelude.map (\n -> False:n) $ createCandidates__ (num-1) constraint vol in
          case compare num vol of
            LT -> []
            EQ -> if L.null xs then [L.replicate x True] else blackList
            GT -> blackList ++ whiteList

volume :: Constraint -> Int
volume (Constraint cs) = sum (intersperse 1 cs)

cvolume :: Constraint -> Int -> Int
cvolume constraint@(Constraint cs) s =
    let box = Prelude.length cs + 1 in
    let ball = s - volume constraint in
    f box ball
    where 
        f _ 0 = 1
        f 1 ball = 1
        f box 1 = box
        f box ball = L.foldl' (\cur elt-> cur + f (box-1) (ball-elt)) 0 [0..ball]

match :: LabelLine -> LabelLine -> MatchLine
match (LabelLine xs) (LabelLine ys) = MatchLine (L.zipWith (\x y -> if x == y then Just x else Nothing) xs ys)

splitConstraint :: Int -> Constraint -> (Constraint, Constraint)
splitConstraint i (Constraint cs) = let (a,b) = L.splitAt i cs in (Constraint a, Constraint b)

createNewRangeConstraint :: RangeConstraint -> [(Int,Int)] -> [RangeConstraint]
createNewRangeConstraint xs [] = [xs]
createNewRangeConstraint (RangeConstraint xs (Range lb ub)) ((c,i):cs) = 
    let (a,b) = splitConstraint c xs in 
    (RangeConstraint a (Range lb (i-1))) : createNewRangeConstraint (RangeConstraint b (Range (i+1) ub)) (Prelude.map (\(n,j) -> (n-c,j)) cs)

solveConstraint :: [Cell] -> RangeConstraint -> Maybe ([Cell], Constraints)
solveConstraint cells rc@(RangeConstraint constraint@(Constraint cs) bound@(Range lb ub)) = if L.null c then Nothing else Just (newCells,Constraints newConstraint)
    where
        targetCells = Prelude.drop (lb-1) $ Prelude.take (ub) cells
        targets = Prelude.map (\(Cell _ ce) -> ce) targetCells
        len = ub - lb + 1
        vol = volume constraint
        c = Prelude.filter (adaptLine (BoardLine targets)) (createCandidates_ len constraint vol)
        candidates = labeling $ head c 
        revCandidates = labeling $ reverseCandidate $ head $ Prelude.filter (adaptLine (BoardLine (Prelude.reverse targets))) (createCandidates_ len (Constraint (Prelude.reverse cs)) vol)
        (MatchLine line) = match candidates revCandidates
        newline = Prelude.map (maybe Nothing (\n -> Just (odd n)) ) line
        newCells = L.foldl' (\cur -> \(Cell i (CellElt c),n) -> if isJust n && c /= n  then (Cell i (CellElt n)):cur else cur) [] (L.zip targetCells newline)
        l = Prelude.map (\(n,i) -> (div (fromJust n) 2, i)) $ Prelude.filter (maybe False even . fst) $ L.zip line [lb..ub]
        newConstraint = Prelude.filter (\(RangeConstraint constraint (Range lb ub)) -> volume constraint /= (ub-lb+1)) $ Prelude.filter (\(RangeConstraint (Constraint cs) _) -> not (L.null cs)) $ createNewRangeConstraint rc l
        reverseCandidate (Candidate c) = Candidate (Prelude.reverse c)

createNewLine :: [Cell] -> CellIndices -> Constraints -> Maybe ([Cell], Constraints)
createNewLine lineCell (CellIndices set) (Constraints constraints) = 
    let (targets, outOfTargets) = L.partition (\(RangeConstraint _ (Range lb ub)) -> any (\n -> member n set) [lb..ub]) constraints in 
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

writeCell :: MBoard -> Direction -> Cell -> IO (Direction, LineIndex, Int)
writeCell (MBoard board) (Direction d) (Cell index@(Point ridx cidx) cell) = do
    writeArray board index cell
    return $ if d then (Direction False,LineIndex cidx, ridx) else (Direction True,LineIndex ridx, cidx)

logicalLinesStep :: MProblem -> Line -> IO (Maybe (MProblem, [(Direction,LineIndex,Int)]))
logicalLinesStep problem@(MProblem mb@(MBoard board) mrc mcc) line@(Line direction num set) = do
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
    
                               
logicalStep :: SolvingProblem -> IO (Maybe MProblem)
logicalStep sp@(SolvingProblem problem (ChangeLines seql)) =
    case viewl seql of 
      EmptyL -> return (Just problem)
      e :< es -> logicalLinesStep problem e >>= maybe (return Nothing) (nextLogicalStep (ChangeLines es))
      where
        insertLine :: ChangeLines -> (Direction,LineIndex,Int) -> ChangeLines
        insertLine (ChangeLines ls) x = ChangeLines (insertLine_ ls x)
        insertLine_ ls x@(xb,xi@(LineIndex xli),xe) = case viewl ls of
            EmptyL -> S.singleton (Line xb xi (CellIndices (T.singleton xe)))
            e@(Line eb ei@(LineIndex eli) es@(CellIndices ecs)) :< ess -> if eb == xb && eli == xli then (Line eb ei (CellIndices (insert xe ecs))) <| ess else e <| (insertLine_ ess x)
        nextLogicalStep :: ChangeLines -> (MProblem, [(Direction,LineIndex,Int)]) -> IO (Maybe MProblem)
        nextLogicalStep es (newProblem,changeLines) = let newLines = L.foldl' insertLine es changeLines in logicalStep (SolvingProblem newProblem newLines)
                      
estimateStep :: MProblem -> IO [SolvingProblem]
estimateStep mproblem@(MProblem (MBoard mb) mrc@(MConstraints rc) mcc@(MConstraints cc)) = do
  rassocs <- getAssocs rc
  cassocs <- getAssocs cc
  let cs = refineConstraint rassocs True ++ refineConstraint cassocs False
  let (bl,li,constraint@(RangeConstraint c (Range lb ub))) = minimumBy (\a b -> compare (f a) (f b)) cs
  rewriteConstraint mrc mcc bl li constraint
  bassocs <- getAssocs mb
  let cells = Prelude.map (\(a,b) -> Cell a b) bassocs
  let targetCell = Prelude.drop(lb-1) $ Prelude.take ub $ createLineFromBoard cells bl li
  let newLines = Prelude.filter (adaptLine (BoardLine (Prelude.map (\(Cell _ ce) -> ce) targetCell))) (createCandidates constraint)
  let newCells = Prelude.map (\(Candidate newLine) -> L.foldl' (\cur -> \((Cell i (CellElt c)),n) -> if isNothing c then (i,n):cur else cur) [] (L.zip targetCell newLine) ) newLines
  iproblem@(IProblem ib irc icc) <- freezeProblem mproblem
  Prelude.mapM ( g iproblem bl ) newCells
    where
      refineConstraint as flg = concatMap (\(i,Constraints cs)-> Prelude.map (\e -> (Direction flg,i,e)) cs) as
--      f (_,_,c) = Prelude.length $ createCandidates c
      f (_,_,(RangeConstraint cs (Range lb ub))) = cvolume cs (ub-lb+1)
      g iproblem direction xs = do
        mp@(MProblem mb _ _) <- thawProblem iproblem
        newLines <- Prelude.mapM (writeCell mb direction) (Prelude.map (\(i,c) -> (Cell i (CellElt (Just c)))) xs)
        return (SolvingProblem mp (ChangeLines (L.foldl' (\lines (bl,i,e)-> (Line bl i (CellIndices (T.singleton e))) <| lines) S.empty newLines)))
      rewriteConstraint_ :: MConstraints -> LineIndex -> RangeConstraint -> IO ()
      rewriteConstraint_ (MConstraints c) li constraint = do
          Constraints cs <- readArray c li
          writeArray c li (Constraints (L.delete constraint cs))
      rewriteConstraint :: MConstraints -> MConstraints -> Direction -> LineIndex -> RangeConstraint -> IO ()
      rewriteConstraint rc cc (Direction d) i constraint = rewriteConstraint_ (if d then rc else cc) i constraint
        
isSolved :: MBoard -> IO Bool
isSolved (MBoard mb) = getElems mb >>= return . and . ( Prelude.map (\(CellElt n) -> isJust n) )

incr :: Depth -> Depth
incr (Depth d) = Depth (d+1)

solve :: Depth -> SolvingProblem -> IO [(Depth,MBoard)]
solve depth@(Depth d) sp = logicalStep sp >>= maybe (return []) next
    where
      next logicProblem@(MProblem mb _ _) = do
        print d >> printArray mb
        isSolved mb >>= bool (estimateStep logicProblem >>= Prelude.mapM (\newp->solve (incr depth) newp) >>= return.concat) (return [(depth,mb)])

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
    createChangeLines_ d w num = (createChangeLines_ d w (num-1)) |> (Line (Direction d) (LineIndex num) (CellIndices (T.fromList [1..w])))

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

