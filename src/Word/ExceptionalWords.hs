module Word.ExceptionalWords (isExceptionalWord) where

-- List of exceptional words
-- Here is a list of words that meet the "stemming conditions" but are meaningful and do not need stemming.
-- Words hardcoded here will be skipped during stemming (note that this list may not be exhaustive).

exceptionalWords :: [String]
exceptionalWords = [
    "atlas",
    "news",
    "series", 
    "species",
    "bus",
    "cosmos",
    "bias"
    ]

isExceptionalWord :: String -> Bool
isExceptionalWord word = elem word exceptionalWords