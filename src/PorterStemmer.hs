module PorterStemmer
  (stem
  ) where

import Word.ExceptionalWords(isExceptionalWord)

isConsonant :: String -> Int -> Bool
isConsonant word i
    | i < 0 || i >= length word = error "Index out of bounds"
    | c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' = False
    | c == 'y' = i == 0 || not (isConsonant word (i - 1))
    | otherwise = True
  where
    c = word !! i

hasVowel :: String -> Int -> Bool
hasVowel word j = any (\i -> not (isConsonant word i)) [0..j]

hasDoubleConsonant :: String -> Int -> Bool
hasDoubleConsonant word j
    | j < 1 = False
    | otherwise = word !! j == word !! (j - 1) && isConsonant word j

measure :: String -> Int -> Int
measure word j = countSequences 0 0
  where
    len = length word
    countSequences n i
        | i > j = n
        | not (isConsonant word i) = countVowels n (i + 1)
        | otherwise = countSequences n (i + 1)
    countVowels n i
        | i > j = n
        | isConsonant word i = countSequences (n + 1) (i + 1)
        | otherwise = countVowels n (i + 1)

isCVC :: String -> Int -> Bool
isCVC word i
    | i < 2 = False
    | not (isConsonant word i) = False
    | isConsonant word (i - 1) = False
    | not (isConsonant word (i - 2)) = False
    | lastConsonant `elem` "wxy" = False
    | otherwise = True
  where
    lastConsonant = word !! i
    
endsWith :: String -> String -> (Bool, Int)
endsWith word suffix
    | suffixLen > wordLen = (False, 0)
    | take suffixLen (drop offset word) == suffix = (True, offset - 1)
    | otherwise = (False, 0)
  where
    wordLen = length word
    suffixLen = length suffix
    offset = wordLen - suffixLen

setTo :: String -> String -> Int -> String
setTo word newSuffix j = take (j + 1) word ++ newSuffix

apply :: String -> String -> Int -> String
apply word newSuffix j =
    if measure word j > 0 then setTo word newSuffix j else word

stem :: String -> String
stem word
  | isExceptionalWord word = word
  | otherwise = step5 (step4 (step3 (step2 (step1c (step1b (step1a word))))))

step1a :: String -> String
step1a word
    | last word == 's' = handleS word
    | otherwise = word
  where
    handleS w
        | (True, j) <- w `endsWith` "sses" = setTo w "ss" j
        | (True, j) <- w `endsWith` "ies" = setTo w "i" j
        | length w > 1 && last (init w) /= 's' = init w
        | otherwise = w

step1b :: String -> String
step1b word
    | (found, j) <- endsWith word "eed", found && measure word j > 0 = setTo word "ee" j
    | (found, j) <- endsWith word "ed", found && hasVowel (take (j + 1) word) j = step1b2 (take (j + 1) word)
    | (found, j) <- endsWith word "ing", found && hasVowel (take (j + 1) word) j = step1b2 (take (j + 1) word)
    | otherwise = word

step1b2 :: String -> String
step1b2 word
    | (found, j) <- endsWith word "at", found = setTo word "ate" j
    | (found, j) <- endsWith word "bl", found = setTo word "ble" j
    | (found, j) <- endsWith word "iz", found = setTo word "ize" j
    | hasDoubleConsonant word (length word - 1) =
        if last word `elem` "lsz"
        then word
        else init word
    | measure word (length word - 1) == 1 && isCVC word (length word - 1) = word ++ "e"
    | otherwise = word

step1c :: String -> String
step1c word
    | (True, j) <- endsWith word "y", hasVowelBeforeY word (j - 1) = setTo word "i" j
    | otherwise = word

hasVowelBeforeY :: String -> Int -> Bool
hasVowelBeforeY word index
    | index < 0 = False
    | otherwise = any (\i -> not (isConsonant word i)) [0..index]
    
step2 :: String -> String
step2 word
    | (found, j) <- endsWith word "ational", found = apply word "ate" j
    | (found, j) <- endsWith word "tional", found = apply word "tion" j
    | (found, j) <- endsWith word "enci", found = apply word "ence" j
    | (found, j) <- endsWith word "anci", found = apply word "ance" j
    | (found, j) <- endsWith word "izer", found = apply word "ize" j
    | (found, j) <- endsWith word "abli", found = apply word "able" j
    | (found, j) <- endsWith word "alli", found = apply word "al" j
    | (found, j) <- endsWith word "entli", found = apply word "ent" j
    | (found, j) <- endsWith word "eli", found = apply word "e" j
    | (found, j) <- endsWith word "ousli", found = apply word "ous" j
    | (found, j) <- endsWith word "ization", found = apply word "ize" j
    | (found, j) <- endsWith word "ation", found = apply word "ate" j
    | (found, j) <- endsWith word "ator", found = apply word "ate" j
    | (found, j) <- endsWith word "alism", found = apply word "al" j
    | (found, j) <- endsWith word "iveness", found = apply word "ive" j
    | (found, j) <- endsWith word "fulness", found = apply word "ful" j
    | (found, j) <- endsWith word "ousness", found = apply word "ous" j
    | (found, j) <- endsWith word "aliti", found = apply word "al" j
    | (found, j) <- endsWith word "iviti", found = apply word "ive" j
    | (found, j) <- endsWith word "biliti", found = apply word "ble" j
    | otherwise = word

step3 :: String -> String
step3 word
    | (found, j) <- endsWith word "icate", found = apply word "ic" j
    | (found, j) <- endsWith word "ative", found = apply word "" j
    | (found, j) <- endsWith word "alize", found = apply word "al" j
    | (found, j) <- endsWith word "iciti", found = apply word "ic" j
    | (found, j) <- endsWith word "ical", found = apply word "ic" j
    | (found, j) <- endsWith word "ful", found = apply word "" j
    | (found, j) <- endsWith word "ness", found = apply word "" j
    | (found, j) <- endsWith word "sion", found = apply word "s" j
    | otherwise = word

step4 :: String -> String
step4 word
    | (found, j) <- endsWith word "al", found = applyIf word "" j
    | (found, j) <- endsWith word "ance", found = applyIf word "" j
    | (found, j) <- endsWith word "ence", found = applyIf word "" j
    | (found, j) <- endsWith word "er", found = applyIf word "" j
    | (found, j) <- endsWith word "ic", found = applyIf word "" j
    | (found, j) <- endsWith word "able", found = applyIf word "" j
    | (found, j) <- endsWith word "ible", found = applyIf word "" j
    | (found, j) <- endsWith word "ant", found = applyIf word "" j
    | (found, j) <- endsWith word "ement", found = applyIf word "" j
    | (found, j) <- endsWith word "ment", found = applyIf word "" j
    | (found, j) <- endsWith word "ent", found = applyIf word "" j
    | (found, j) <- endsWith word "ou", found = applyIf word "" j
    | (found, j) <- endsWith word "ism", found = applyIf word "" j
    | (found, j) <- endsWith word "ate", found = applyIf word "" j
    | (found, j) <- endsWith word "iti", found = applyIf word "" j
    | (found, j) <- endsWith word "ous", found = applyIf word "" j
    | (found, j) <- endsWith word "ive", found = applyIf word "" j
    | (found, j) <- endsWith word "ize", found = applyIf word "" j
    | (found, j) <- endsWith word "hood", found = applyIf word "" j
    | otherwise = word
  where
    applyIf :: String -> String -> Int -> String
    applyIf word suffix j
        | measure word j > 1 = setTo word suffix j
        | otherwise = word

step5 :: String -> String
step5 word
    | (found, j) <- endsWith word "e", found = 
        let stemmed = apply word "" j
            m = measure stemmed (length stemmed - 1)
        in if m > 1 
           then stemmed
           else if m == 1 && not (isCVC stemmed (length stemmed - 1))
                then stemmed
                else word
    | hasDoubleConsonant word (length word - 1) && last word == 'l' = init word
    | otherwise = word