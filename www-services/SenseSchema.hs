{-# LANGUAGE DeriveDataTypeable #-}

module SenseSchema where

import PGF2
import Database.Helda
import Data.Data
import Data.List(nub)
import Interval

type SynsetOffset = String

data Synset
  = Synset
      { synsetOffset :: SynsetOffset
      , parents      :: [Key Synset]
      , children     :: Interval (Key Synset)
      , gloss        :: String
      }
    deriving (Data,Ord,Eq,Show)

data Status
  = Guessed | Unchecked | Changed | Checked
  deriving (Data,Show)

data Lexeme
  = Lexeme 
      { lex_fun     :: Fun
      , lex_defs    :: [(String,String,Status)]
      , synset      :: Maybe (Key Synset)
      , domains     :: [String]
      , images      :: [String]
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
lexemes_synset = maybeIndex lexemes "synset" synset

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

checked :: Table (Fun,String)
checked = table "checked"
