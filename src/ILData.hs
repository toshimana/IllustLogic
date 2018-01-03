module ILData where

import Data.Array.IO
import Data.Set as T
import Data.Sequence as S

data Point = Point Int Int deriving (Ix, Ord, Eq, Show)
data Range = Range Int Int deriving (Eq, Show)

newtype CellElt = CellElt (Maybe Bool) deriving (Eq, Show)
data Cell = Cell Point CellElt deriving (Eq, Show)

newtype Constraint = Constraint [Int] deriving (Eq, Show)
data RangeConstraint = RangeConstraint Constraint Range Candidates Candidates deriving (Eq, Show)

newtype CellIndex = CellIndex Int deriving (Ord, Eq)
instance Num CellIndex where
  (+) (CellIndex x) (CellIndex y) = CellIndex (x + y)
  (-) (CellIndex x) (CellIndex y) = CellIndex (x - y)
  (*) (CellIndex x) (CellIndex y) = CellIndex (x * y)
  negate (CellIndex x) = CellIndex (negate x)
  abs (CellIndex x) = CellIndex (abs x)
  signum (CellIndex x) = CellIndex (signum x)
  fromInteger a = CellIndex (fromInteger a)

newtype Constraints = Constraints [RangeConstraint] deriving (Eq, Show)
    
newtype Candidate = Candidate [Bool] deriving (Show,Eq)
newtype Candidates = Candidates [Candidate] deriving (Eq, Show)
    
newtype LineIndex = LineIndex Int deriving (Ix, Ord, Eq)
newtype Direction = Direction Bool deriving (Eq)
newtype CellIndices = CellIndices (Set CellIndex)
data Line = Line Direction LineIndex CellIndices

newtype BoardLine = BoardLine [CellElt] deriving (Eq, Show)
data LinePosition = LinePosition Direction LineIndex CellIndex

newtype MConstraints = MConstraints (IOArray LineIndex Constraints)
newtype MBoard = MBoard (IOArray Point CellElt)
data MProblem = MProblem MBoard MConstraints MConstraints
newtype ChangeLines = ChangeLines (Seq Line)
data SolvingProblem = SolvingProblem MProblem ChangeLines
    
