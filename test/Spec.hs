import Test.Hspec
import Lib
import Data.Set
    
spec :: Spec
spec = do
  describe "createCandidates" $ do
    it "simple" $ do
      (createCandidates ([3],(1,3))) `shouldBe` [[True,True,True]]
    it "multi" $ do
      (createCandidates ([3],(1,5))) `shouldBe` [[True,True,True,False,False],[False,True,True,True,False],[False,False,True,True,True]]

  describe "solveConstraint" $ do
    it "test1" $ do
      (solveConstraint (zip [(1,x)|x<-[1..]] [Nothing,Nothing,Nothing,Nothing,Nothing]) ([3],(1,5))) `shouldBe` (Just ([((1,3),Just True)],[([3],(1,5))]))
    it "test2" $ do
      (solveConstraint (zip [(1,x)|x<-[1..]] [Just False,Nothing,Nothing,Just False,Nothing,Nothing]) ([1,1],(2,6))) `shouldBe` (Just ([],[([1],(2,3)),([1],(5,6))]))
    it "test3" $ do
      (solveConstraint (zip [(1,x)|x<-[1..]] [Just False,Nothing,Nothing,Just False]) ([1],(1,4))) `shouldBe` (Just ([],[([1],(2,3))]))
    it "test4" $ do
      (solveConstraint (zip [(1,x)|x<-[1..]] [Just False, Just True, Just False, Just True, Just False, Just True]) ([1,1,1],(2,6))) `shouldBe` (Just ([],[]))
    it "test5" $ do
      (solveConstraint (zip [(1,x)|x<-[1..]] [Nothing, Nothing, Nothing, Just False, Nothing]) ([1,1],(1,5))) `shouldBe` (Just ([],[([1,1],(1,5))]))

  describe "createNewLine" $ do
    it "test1" $ do
      (createNewLine (zip [(1,x)|x<-[1..]] [Nothing,Nothing,Just True,Nothing,Nothing]) (fromList [3]) [([3],(1,5))]) `shouldBe` (Just ([],[([3],(1,5))]))
    it "test1" $ do
      (createNewLine (zip [(1,x)|x<-[1..]] [Nothing,Nothing,Just True,Nothing,Just False,Just True,Nothing]) (fromList [6]) [([3],(1,4)),([1],(6,7))]) `shouldBe` (Just ([((1,7),Just False)],[([3],(1,4))]))

main :: IO ()
main = hspec spec

                    
