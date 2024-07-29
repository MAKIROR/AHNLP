# Anqiao's Natural Language Processing Library
A natural language processing library based on Haskell (Just a project for study)
<hr>

# Tokenization

This module provides functions for tokenizing strings, including splitting by whitespace, delimiting sentences with specified characters, and segmenting strings based on multiple delimiters.

## Exported Functions

The module exports the following functions:

- `tokenize`
- `tokenizeSentences`
- `tokenizeWithDelimiters`

## Function Details

### `tokenize`

```haskell
tokenize :: String -> [String]
```

<hr>

# Stemmer

This module provides an implementation of the Porter Stemming Algorithm, which reduces English words to their root forms. This is useful for natural language processing tasks such as text analysis and information retrieval.

## Exported Functions

The module exports the following function:

- `stem`

## Function Details

### `stem`

```haskell
stem :: String -> String

