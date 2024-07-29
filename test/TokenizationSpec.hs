module TokenizationSpec where

import Test.Hspec
import Tokenization

spec :: Spec
spec = do
  describe "Tokenization" $ do
    it "tokenizes text into words" $ do
      tokenize "Hello world!" `shouldBe` ["Hello", "world!"]

    it "tokenizes text into sentences" $ do
      tokenizeSentences "Hello world! How are you? I am fine." `shouldBe`
        ["Hello world", "How are you", "I am fine"]

    it "tokenizes text with custom delimiters including spaces, commas, and exclamation marks" $ do
      tokenizeWithDelimiters [' ', ',', '!'] "Hello,world! How,are you!" `shouldBe`
        ["Hello", "world", "How", "are", "you"]