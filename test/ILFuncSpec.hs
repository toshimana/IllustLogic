module ILFuncSpec where

import Test.Hspec

import ILFunc
import ILData

spec :: Spec
spec = do
  describe "createCandidates" $ do
    it "simple" $ do
      (createCandidatesFromRangeConstraint (rangeConstraint [3] (1,3))) `shouldBe` [Candidate [True,True,True]]
    it "multi" $ do
      (createCandidatesFromRangeConstraint (rangeConstraint [3] (1,5))) `shouldBe` [Candidate [True,True,True,False,False],Candidate [False,True,True,True,False],Candidate [False,False,True,True,True]]

  
