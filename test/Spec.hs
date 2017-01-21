import Test.Hspec
import Lib
    
spec :: Spec
spec = do
  describe "createCandidates_" $ do
         it "simple" $ do
           (createCandidates_ 3 [3]) `shouldBe` [[True,True,True]]
         it "multi" $ do
           (createCandidates_ 5 [3]) `shouldBe` [[True,True,True,False,False],[False,True,True,True,False],[False,False,True,True,True]]
                                     
main :: IO ()
main = hspec spec

                    
