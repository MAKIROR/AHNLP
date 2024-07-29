module Tokenization
  ( tokenize
  , tokenizeSentences
  , tokenizeWithDelimiters
  ) where

import Data.List (words, break, dropWhile, dropWhileEnd)
import Data.Char (isSpace)

-- | Tokenize a string into words based on whitespace.
tokenize :: String -> [String]
tokenize text = words text

-- | Tokenize a string into sentences based on specified delimiters.
tokenizeSentences :: String -> [String]
tokenizeSentences text = filter (not . null) (map trim (splitOnDelimiters ".!?" text))
  where
    splitOnDelimiters :: [Char] -> String -> [String]
    splitOnDelimiters delimiters s = go s
      where
        go [] = []
        go xs =
          let (token, rest) = break (`elem` delimiters) xs
              rest' = dropWhile (`elem` delimiters) rest
          in token : go rest'
    
    trim = dropWhile (== ' ') . dropWhileEnd (== ' ')

-- | Tokenize a string into segments based on a list of delimiters.
tokenizeWithDelimiters :: [Char] -> String -> [String]
tokenizeWithDelimiters delimiters text = filter (not . null) (splitOnDelimiters delimiters text)
  where
    splitOnDelimiters :: [Char] -> String -> [String]
    splitOnDelimiters delimiters str = go str
      where
        go [] = []
        go xs =
          let (token, rest) = span (`notElem` delimiters) xs
              rest' = dropWhile (`elem` delimiters) rest
          in token : go rest'