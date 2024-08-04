{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import PorterStemmer

main :: IO ()
main = hspec $ do
  describe "Porter Stemmer" $ do
    it "correctly handles common suffixes" $ do
      stem "hopping" `shouldBe` "hope"
      stem "caving" `shouldBe` "cave"
      
      stem "conditional" `shouldBe` "condition"
      stem "rational" `shouldBe` "ration"
      stem "valence" `shouldBe` "valenc"
      stem "hesitancy" `shouldBe` "hesit"
      stem "digitizer" `shouldBe` "digit"
      stem "conformity" `shouldBe` "conform"
      stem "radical" `shouldBe` "radic"
      stem "activate" `shouldBe` "activ"
      stem "angulariti" `shouldBe` "angular"
      stem "homologous" `shouldBe` "homolog"
      stem "effective" `shouldBe` "effect"
      stem "bowdlerize" `shouldBe` "bowdler"
      stem "happily" `shouldBe` "happili"
      stem "relational" `shouldBe` "relat"
      stem "derivation" `shouldBe` "deriv"
      stem "generation" `shouldBe` "gener"
      stem "precision" `shouldBe` "precis"
      stem "allowance" `shouldBe` "allow"
      stem "adjustment" `shouldBe` "adjust"

    it "does not alter words not matching any suffixes" $ do
      stem "banana" `shouldBe` "banana"
      stem "apple" `shouldBe` "appl"
      stem "orange" `shouldBe` "orang"
      stem "grape" `shouldBe` "grape"
      stem "watermelon" `shouldBe` "watermelon"

    it "handles exceptions correctly" $ do
      stem "atlas" `shouldBe` "atlas"
      stem "cosmos" `shouldBe` "cosmos"
      stem "bias" `shouldBe` "bias"
      stem "analysis" `shouldBe` "analysi"
      stem "basis" `shouldBe` "basi"