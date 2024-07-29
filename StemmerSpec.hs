module StemmerSpec where

import Test.Hspec
import Stemmer (stem)

spec :: Spec
spec = do
  describe "Porter Stemmer" $ do
    it "correctly handles common suffixes" $ do
      stem "relational" `shouldBe` "relate"
      stem "conditional" `shouldBe` "condition"
      stem "rational" `shouldBe` "rational"
      stem "valence" `shouldBe` "valence"
      stem "hesitancy" `shouldBe` "hesitance"
      stem "digitizer" `shouldBe` "digitize"
      stem "conformity" `shouldBe` "conform"
      stem "radical" `shouldBe` "radic"
      stem "activate" `shouldBe` "activ"
      stem "angulariti" `shouldBe` "angular"
      stem "homologous" `shouldBe` "homolog"
      stem "effective" `shouldBe` "effect"
      stem "bowdlerize" `shouldBe` "bowdler"

    it "does not alter words not matching any suffixes" $ do
      stem "apple" `shouldBe` "apple"
      stem "banana" `shouldBe` "banana"
      stem "cherry" `shouldBe` "cherry"
      stem "study" `shouldBe` "study"

    it "handles exceptions correctly" $ do
      stem "atlas" `shouldBe` "atlas"
      stem "news" `shouldBe` "news"
      stem "series" `shouldBe` "series"
      stem "species" `shouldBe` "species"
      stem "bus" `shouldBe` "bus"
      stem "class" `shouldBe` "class"
      stem "glass" `shouldBe` "glass"
      stem "kiss" `shouldBe` "kiss"
      stem "miss" `shouldBe` "miss"
      stem "dress" `shouldBe` "dress"
      stem "loss" `shouldBe` "loss"
      stem "process" `shouldBe` "process"
      stem "cross" `shouldBe` "cross"
      stem "address" `shouldBe` "address"