{-# LANGUAGE DeriveDataTypeable #-}

module SenseSchema where

import PGF2
import Database.Helda
import Data.Data
import Data.List(nub)

type SynsetOffset = String

data Synset
  = Synset
      { synsetOffset :: SynsetOffset
      , gloss        :: String
      }
    deriving (Data,Ord,Eq,Show)

data Lexeme
  = Lexeme 
      { lex_fun     :: Fun
      , synset      :: Key Synset
      , domains     :: [String]
      , example_ids :: [Key Expr]
      }
    deriving (Data,Show)

data Embedding
  = Embedding
      { emb_fun :: Fun
      , hvec    :: [Double]
      , mvec    :: [Double]
      }
    deriving (Data,Show)

synsets :: Table Synset
synsets = table "synsets"

lexemes :: Table Lexeme
lexemes = table "lexemes"
             `withIndex` lexemes_fun
             `withIndex` lexemes_synset

lexemes_fun :: Index Lexeme Fun
lexemes_fun = index lexemes "fun" lex_fun

lexemes_synset :: Index Lexeme (Key Synset)
lexemes_synset = index lexemes "synset" synset

coefficients :: Table [Double]
coefficients = table "coefficients"

embeddings :: Table Embedding
embeddings = table "embeddings"
                `withIndex` embeddings_fun

embeddings_fun :: Index Embedding Fun
embeddings_fun = index embeddings "fun" emb_fun

examples :: Table Expr
examples = table "examples"
             `withIndex` examples_fun

examples_fun :: Index Expr Fun
examples_fun = listIndex examples "fun" (nub . exprFunctions)

checked :: Table Fun
checked = table "checked"
