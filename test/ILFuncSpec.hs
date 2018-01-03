module ILFuncSpec where

import Test.Hspec

import ILFunc
import ILData

spec :: Spec
spec = do
  describe "createCandidatesFromCandidates" $ do
         it "test1" $ do
           (createCandidatesFromCandidate (Constraint [1]) (Candidate [False,True,False])) `shouldBe` (Candidates [Candidate [False,True,False], Candidate [False, False, True]])

         it "test2" $ do
           (createCandidatesFromCandidate (Constraint [1,1]) (Candidate [False,True,False,False,True,False])) `shouldBe` (Candidates [Candidate [False,True,False,False,True,False],Candidate [False,True,False,False,False,True],Candidate [False,False,True,False,True,False],Candidate [False,False,True,False,False,True],Candidate [False,False,False,True,False,True]])
