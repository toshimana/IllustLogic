import Test.Hspec
import Lib
import Data.Set

rangeConstraint :: [Int] -> (Int,Int) -> RangeConstraint
rangeConstraint constraint (r1,r2) = RangeConstraint (Constraint constraint) (Range r1 r2)

spec :: Spec
spec = do
  describe "createCandidates" $ do
    it "simple" $ do
      (createCandidatesFromRangeConstraint (rangeConstraint [3] (1,3))) `shouldBe` [Candidate [True,True,True]]
    it "multi" $ do
      (createCandidatesFromRangeConstraint (rangeConstraint [3] (1,5))) `shouldBe` [Candidate [True,True,True,False,False],Candidate [False,True,True,True,False],Candidate [False,False,True,True,True]]

  describe "solveConstraint" $ do
    it "test1" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing,Nothing,Nothing,Nothing,Nothing])) (rangeConstraint [3] (1,5))
      let expect = Just ([Cell (Point 1 3) (CellElt (Just True))],Constraints [rangeConstraint [3] (1,5)])
      actual `shouldBe` expect 
    it "test2" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Just False,Nothing,Nothing,Just False,Nothing,Nothing])) (rangeConstraint [1,1] (2,6))
      let expect = Just ([],Constraints [rangeConstraint [1] (2,3),rangeConstraint [1] (5,6)])
      actual `shouldBe` expect
    it "test3" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Just False,Nothing,Nothing,Just False])) (rangeConstraint [1] (1,4))
      let expect = Just ([],Constraints [rangeConstraint [1] (2,3)])
      actual `shouldBe` expect
    it "test4" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Just False, Just True, Just False, Just True, Just False, Just True])) (rangeConstraint [1,1,1] (2,6))
      let expect = Just ([],Constraints [])
      actual `shouldBe` expect
    it "test5" $ do
      let actual = solveConstraint (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing, Nothing, Nothing, Just False, Nothing])) (rangeConstraint [1,1] (1,5))
      let expect = Just ([],Constraints [rangeConstraint [1,1] (1,5)])
      actual `shouldBe` expect

  describe "createNewLine" $ do
    it "test1" $ do
      let actual = createNewLine (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing,Nothing,Just True,Nothing,Nothing])) (CellIndices (fromList [3])) (Constraints [rangeConstraint [3] (1,5)])
      let expect = Just ([],Constraints [rangeConstraint [3] (1,5)])
      actual `shouldBe` expect
    it "test1" $ do
      let actual = createNewLine (zipWith Cell [Point 1 x|x<-[1..]] (Prelude.map CellElt [Nothing,Nothing,Just True,Nothing,Just False,Just True,Nothing])) (CellIndices $ fromList [6]) (Constraints [rangeConstraint [3] (1,4),rangeConstraint [1] (6,7)])
      let expect = Just ([Cell (Point 1 7) (CellElt $ Just False)],Constraints [rangeConstraint [3] (1,4)])
      actual `shouldBe` expect

main :: IO ()
main = hspec spec

                    
