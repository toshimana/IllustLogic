module LibSpec where

import Test.Hspec
                    
spec :: Spec
spec = do
  describe "dummy" $ do
         it "dummy" $ do
                       0 `shouldBe` 0
