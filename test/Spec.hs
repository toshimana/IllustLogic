import Test.Hspec
import Lib
    
spec :: Spec
spec = do
  describe "createCandidates_" $ do
    it "simple" $ do
      (createCandidates ([3],(1,3))) `shouldBe` [[True,True,True]]
    it "multi" $ do
      (createCandidates ([3],(1,5))) `shouldBe` [[True,True,True,False,False],[False,True,True,True,False],[False,False,True,True,True]]

  describe "solveConstraint" $ do
    it "test1" $ do
      (solveConstraint (zip [(1,x)|x<-[1..5]] [Nothing,Nothing,Nothing,Nothing,Nothing]) ([3],(1,5))) `shouldBe` (Just ([((1,3),Just True)],[([3],(1,5))]))
    it "test2" $ do
      (solveConstraint (zip [(1,x)|x<-[1..6]] [Just False,Nothing,Nothing,Just False,Nothing,Nothing]) ([1,1],(2,6))) `shouldBe` (Just ([],[([1],(2,3)),([1],(5,6))]))

main :: IO ()
main = hspec spec

                    
