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

type Line = (Bool,Int) -- (direction::Bool, Number::Int)

type Candidate = Integer
type Candidates = Either Constraint [Candidate]
type MCandidates = IOArray Int Candidates
type ICandidates = Array Int Candidates

type MProblem = (MBoard, MCandidates, MCandidates)
type IProblem = (IBoard, ICandidates, ICandidates)
    
inputdata :: [String] -> (Constraints,Constraints)
inputdata cs = 
    (map g rowStr, map g colStr)
    where
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
              
createCandidates :: Int -> Constraint -> [Candidate]
createCandidates num [] = [0]
createCandidates num constraint@(x:xs) =
    case compare num (volume constraint) of
      LT -> []
      EQ -> blackList
      GT -> blackList ++ whiteList
     where 
       blackList = map (\n -> (shiftL 1 x)-1 + (shiftL 1 (x+1))*n) $ createCandidates (num-x-1) xs
       whiteList = map (\n -> shiftL n 1) $ createCandidates (num-1) constraint

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

createNewLine :: MCandidates -> Int -> [(Index,Cell)] -> IO (Maybe [(Index,Cell)])
createNewLine candidates num lineCell =
    let len = Prelude.length lineCell in
    let match a b = if a == b then Just (odd a) else Nothing in
    let currentCells = map snd lineCell in
    do
      constElt <- readArray candidates num
      ret <- case constElt of
        Left constraints ->
          let can = createCandidates_ len constraints in
          let c = Prelude.filter (\n -> adaptLine_ currentCells n) can in
          let headLine = labeling $ head c in
          let cc = Prelude.filter (\n -> adaptLine_ (L.reverse currentCells) n) (createCandidates_ len (L.reverse constraints)) in
          let tailLine = labeling $ L.reverse $ head cc in
          if L.null c then return Nothing
          else do
            return (Just (headLine,tailLine))
        Right c -> do
          let newLine = adaptLines (map snd lineCell) c
          let headLine = labeling $ toBoolList len $ head newLine
          let tailLine = labeling $ toBoolList len $ last newLine
          if L.null newLine then return Nothing
          else do
            writeArray candidates num (Right newLine)
            return (Just (headLine,tailLine))
      case ret of
        Nothing -> return Nothing
        Just (h,t) -> 
            let line = L.zipWith match h t in
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
convertCandidate constraint line = map toCandidate $ Prelude.filter (adaptLine_ line) $ createCandidates_ (Prelude.length line) constraint

createEstimateBoards :: IProblem-> (IProblem, Bool,Int,[(Candidate,[(Index,Cell)],Seq Line)])
createEstimateBoards (ib,irc,icc) =
    let rowCandidates = L.zipWith (\a -> \b -> (True, a,b)) (indices irc) (elems irc) in 
    let colCandidates = L.zipWith (\a -> \b -> (False,a,b)) (indices icc) (elems icc) in
    let uncompletedRowCandidates = Prelude.filter (\(_,_,n) -> isUncomplete n) rowCandidates in
    let uncompletedColCandidates = Prelude.filter (\(_,_,n) -> isUncomplete n) colCandidates in
    let uncompletedCandidates = [head uncompletedRowCandidates, last uncompletedRowCandidates, head uncompletedColCandidates, last uncompletedColCandidates] in
    let uncompletedCandidates_ = map (\(bl,a,b)-> let len = Prelude.length (g bl) in (bl,a,f bl a b len)) uncompletedCandidates in
    let minimumCandidates = L.foldl1' (\a@(_,_,aa) -> \b@(_,_,bb) -> if (Prelude.length aa) <= (Prelude.length bb) then a else b) uncompletedCandidates_ in
    createNewBoards minimumCandidates
        where
          g :: Bool -> ICandidates
          g bl = if bl then irc else icc
          isUncomplete (Left _) = True
          isUncomplete (Right xs) = 1 < (Prelude.length xs)
          f :: Bool -> Int -> Either [Int] [Candidate] -> Int -> [Candidate]
          f bl a (Left cs) len = let cells = map snd (toLineIndex bl a) in map toCandidate $ Prelude.filter (adaptLine_ cells) $ createCandidates_ len cs
          f _ _ (Right xs) _ = xs 
          newRc = convertCandidates True irc
          newCc = convertCandidates False icc
          toLineIndex d n = createLineFromBoard (assocs ib) d n
          h :: ([(Int,Candidates)],[(Int,Candidates)]) -> (Bool,Int,[Candidate]) -> ([(Int,Candidates)],[(Int,Candidates)])
          h (rr,cc) (bl,i,c) = let cs = Right c in if bl then ((i,cs):rr,cc) else (rr,(i,cs):cc)
          createNewCandidates xs = let (r,c) = foldl h ([],[]) xs in (irc // r, icc // c)
          convertCandidates d candidates =
              let as = assocs candidates in
              if (isRight $ snd.head $ as ) then candidates
              else listArray (bounds candidates) $ map (\(i,Left c) -> Right $ convertCandidate c (map snd (toLineIndex d i))) as
          createNewBoard :: Bool -> [(Index,Cell)] -> Candidate -> (Candidate,[(Index,Cell)],Seq Line)
          createNewBoard direction line candidate =
              let cells = map (\n -> Just n) (toBoolList (Prelude.length line) candidate) in
              let newCells = rewriteLine (L.zip line cells) in
              let f = if direction then snd else fst in
              let updateLines = map (\(index,cell)-> (not direction, f index) ) newCells in
              (candidate,newCells,S.fromList updateLines)
          createNewBoards (direction, lineNo, candidates) =
              let currentLine = toLineIndex direction lineNo in
              ((ib,newRc,newCc),direction, lineNo, map (createNewBoard direction currentLine) candidates)
        

estimateStep :: MProblem -> IO [(MProblem,Seq Line)]
estimateStep mproblem = do
  iproblem <- freezeProblem mproblem
  let (newProblem, direction,lineNo,ret) = (createEstimateBoards iproblem)
  mapM (f newProblem direction lineNo) ret
    where
      f :: IProblem -> Bool -> Int -> (Candidate,[(Index,Cell)],Seq Line) -> IO (MProblem,Seq Line)
      f problem direction lineNo (newLine, newCells, updateLine) = do
        newproblem@(mb,mrc,mcc) <- thawProblem problem
        mapM_ (\(i,c) -> writeArray mb i c) newCells
        let constraint = if direction then mrc else mcc
        writeArray constraint lineNo (Right [newLine])
        return (newproblem,updateLine)

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
    if let f = sum.concat in f rc /= f cc then print "Input Data Failed."
    else do
        rowConstraint <- newListArray (1,Prelude.length rc) (map (\n->Left n) rc)
        colConstraint <- newListArray (1,Prelude.length cc) (map (\n->Left n) cc)
        mb <- newArray ((1,1),(Prelude.length rc, Prelude.length cc)) Nothing
        let allLine = (createLineSeq True (Prelude.length rc)) >< (createLineSeq False (Prelude.length cc))
        results <- solve 0 (mb, rowConstraint, colConstraint) allLine
        print "[[Result]]"
        mapM_ (\(d,b) -> do
            print d
            printArray b) results

