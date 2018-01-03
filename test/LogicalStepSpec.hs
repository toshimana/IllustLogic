module LogicalStepSpec where

import Test.Hspec
import Data.Set

import LogicalStep
import ILFunc
import ILData
    
spec :: Spec
spec = do
  describe "divideRangeConstraint" $ do
    it "test1" $ do
         let fc = Candidates [Candidate [False,True,False,False],Candidate [False,False,True,False]]
         let rc = Candidates [Candidate [False,False,True,False],Candidate [False,True,False,False]]
         let actual = divideRangeConstraint (RangeConstraint (Constraint [1]) (Range 1 4) fc rc) [(ConstraintIndex 0,CellIndex 1),(ConstraintIndex 1, CellIndex 4)]
         let expect = [RangeConstraint (Constraint []) (Range 1 0) (Candidates [Candidate []]) (Candidates [Candidate []]),RangeConstraint (Constraint [1]) (Range 2 3) (Candidates [Candidate [True,False],Candidate [False,True]]) (Candidates [Candidate [False,True],Candidate [True,False]]),RangeConstraint (Constraint []) (Range 5 4) (Candidates [Candidate []]) (Candidates [Candidate []])]
         actual `shouldBe` expect 

  describe "solveConstraint" $ do
    it "test1" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing,Nothing,Nothing,Nothing,Nothing])) (createRangeConstraint [3] (Range 1 5) )
      let expect = Just ([Cell (Point 1 3) (CellElt (Just True))],Constraints [createRangeConstraint [3] (Range 1 5)])
      actual `shouldBe` expect 

    it "test2" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Just False,Nothing,Nothing,Just False,Nothing,Nothing])) (createRangeConstraint [1,1] (Range 2 6))
      let expect = Just ([],Constraints [createRangeConstraint [1] (Range 2 3), createRangeConstraint [1] (Range 5 6)])
      actual `shouldBe` expect

    it "test3" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Just False,Nothing,Nothing,Just False])) (createRangeConstraint [1] (Range 1 4))
      let expect = Just ([],Constraints [createRangeConstraint [1] (Range 2 3)])
      actual `shouldBe` expect

    it "test4" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Just False, Just True, Just False, Just True, Just False, Just True])) (createRangeConstraint [1,1,1] (Range 2 6))
      let expect = Just ([],Constraints [])
      actual `shouldBe` expect

    it "test5" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing, Nothing, Nothing, Just False, Nothing])) (createRangeConstraint [1,1] (Range 1 5))
      let expect = Just ([],Constraints [createRangeConstraint [1,1] (Range 1 5)])
      actual `shouldBe` expect

  describe "createNewLine" $ do
    it "test1" $ do
      let actual = createNewLine (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing,Nothing,Just True,Nothing,Nothing])) (CellIndices (fromList [3])) (Constraints [createRangeConstraint [3] (Range 1 5)])
      let expect = Just ([],Constraints [createRangeConstraint [3] (Range 1 5)])
      actual `shouldBe` expect

    it "test1" $ do
      let actual = createNewLine (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing,Nothing,Just True,Nothing,Just False,Just True,Nothing])) (CellIndices $ fromList [6]) (Constraints [createRangeConstraint [3] (Range 1 4),createRangeConstraint [1] (Range 6 7)])
      let expect = Just ([Cell (Point 1 7) (CellElt $ Just False)],Constraints [createRangeConstraint [3] (Range 1 4)])
      actual `shouldBe` expect
