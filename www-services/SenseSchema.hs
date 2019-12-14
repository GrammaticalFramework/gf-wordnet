{-# LANGUAGE DeriveDataTypeable #-}

module SenseSchema where

import PGF2
import Database.Daison
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
      , status      :: [(String,Status)]
      , synset      :: Maybe (Key Synset)
      , domains     :: [String]
      , images      :: [(String,String)]
      , example_ids :: [Key Expr]
      , frame_ids   :: [Key Frame]
      }
    deriving (Data,Show)

data Embedding
  = Embedding
      { emb_fun :: Fun
      , hvec    :: [Double]
      , mvec    :: [Double]
      }
    deriving (Data,Show)

data Class
  = Class
      { name     :: String
      , vars     :: [(String,[String])]
      , super_id :: Maybe (Key Class)
      }
   deriving (Data,Show)

data Frame
  = Frame
      { class_id :: Key Class
      , pattern  :: Expr
      }
   deriving (Data,Show)

data Update
  = Update
     { user   :: String
     , lex_id :: Fun
     , lang   :: String
     , def    :: String
     }
   deriving (Data,Show)

synsets :: Table Synset
synsets = table "synsets"

lexemes :: Table Lexeme
lexemes = table "lexemes"
             `withIndex` lexemes_fun
             `withIndex` lexemes_synset
             `withIndex` lexemes_domain
             `withIndex` lexemes_frame

lexemes_fun :: Index Lexeme Fun
lexemes_fun = index lexemes "fun" lex_fun

lexemes_synset :: Index Lexeme (Key Synset)
lexemes_synset = maybeIndex lexemes "synset" synset

lexemes_domain :: Index Lexeme String
lexemes_domain = listIndex lexemes "domain" domains

lexemes_frame :: Index Lexeme (Key Frame)
lexemes_frame = listIndex lexemes "frames" frame_ids

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

classes :: Table Class
classes = table "classes"
            `withIndex` classes_super
            `withForeignKey` classes_super

classes_super :: Index Class (Key Class)
classes_super = maybeIndex classes "super" super_id

frames :: Table Frame
frames = table "frames"
           `withIndex` frames_class
           `withForeignKey` lexemes_frame

frames_class :: Index Frame (Key Class)
frames_class = index frames "super" class_id

updates :: Table Update
updates = table "updates"
            `withIndex` updates_idx
            `withIndex` updates_usr

updates_idx :: Index Update (String,Fun,String)
updates_idx = index updates "idx" (\u -> (user u,lex_id u,lang u))

updates_usr :: Index Update String
updates_usr = index updates "usr" user
