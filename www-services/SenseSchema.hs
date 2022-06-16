{-# LANGUAGE DeriveDataTypeable #-}

module SenseSchema where

import PGF2
import Database.Daison
import Data.Data
import Data.List(nub)
import Interval

type SynsetOffset = String

data DomainType
  = Topic
  | Region
  | Usage
  deriving (Data,Eq,Show,Read)

data HolonymyType
  = Member
  | Substance
  | Part
  deriving (Data,Eq,Show,Read)

data PointerSymbol
  = Antonym
  | Hypernym
  | InstanceHypernym
  | Hyponym
  | InstanceHyponym
  | Holonym HolonymyType
  | Meronym HolonymyType
  | Attribute
  | DomainOfSynset DomainType
  | MemberOfDomain DomainType
  | Entailment
  | Cause
  | AlsoSee
  | VerbGroup
  | SimilarTo
  | Derived
  | Participle
  deriving (Data,Eq,Show,Read)

inversePointer Antonym             = Just Antonym
inversePointer Hypernym            = Just Hyponym
inversePointer InstanceHypernym    = Just InstanceHyponym
inversePointer Hyponym             = Just Hypernym
inversePointer InstanceHyponym     = Just InstanceHypernym
inversePointer (Holonym ht)        = Just (Meronym ht)
inversePointer (Meronym ht)        = Just (Holonym ht)
inversePointer Attribute           = Just Attribute
inversePointer (DomainOfSynset dt) = Just (MemberOfDomain dt)
inversePointer (MemberOfDomain dt) = Just (DomainOfSynset dt)
inversePointer VerbGroup           = Just VerbGroup
inversePointer SimilarTo           = Just SimilarTo
inversePointer Derived             = Just Derived
inversePointer _                   = Nothing

data Synset
  = Synset
      { synsetOffset :: SynsetOffset
      , pointers     :: [(PointerSymbol,Key Synset)]
      , children     :: Interval (Key Synset)
      , gloss        :: String
      , images       :: [(String,String)]
      }
    deriving (Data,Show)

data Status
  = Guessed | Unchecked | Changed | Checked
  deriving (Data,Show,Eq)

data Domain
  = Domain
      { domain_name   :: String
      , domain_is_dim :: Bool
      , domain_parent :: Key Domain
      }
    deriving Data

data Lexeme
  = Lexeme
      { lex_fun      :: Fun
      , lex_prob     :: Float
      , status       :: [(String,Status)]
      , synset       :: Maybe (Key Synset)
      , domain_ids   :: [Key Domain]
      , example_ids  :: [Key Expr]
      , frame_ids    :: [Key Frame]
      , lex_pointers :: [(PointerSymbol,Key Lexeme)]
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
      { class_id      :: Key Class
      , base_class_id :: Key Class
      , pattern       :: Expr
      , semantics     :: String
      }
   deriving (Data,Show)

type FrameInstance = (Key Frame,[(String,FId)])

synsets :: Table Synset
synsets = table "synsets"

domains :: Table Domain
domains = table "domains"
             `withIndex` domains_parent

domains_parent :: Index Domain (Key Domain)
domains_parent = index domains "parent" domain_parent

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

lexemes_domain :: Index Lexeme (Key Domain)
lexemes_domain = listIndex lexemes "domain" domain_ids

lexemes_frame :: Index Lexeme (Key Frame)
lexemes_frame = listIndex lexemes "frames" frame_ids

examples :: Table (Expr,[FrameInstance])
examples = table "examples"
             `withIndex` examples_fun

examples_fun :: Index (Expr,[FrameInstance]) Fun
examples_fun = listIndex examples "fun" (nub . exprFunctions . fst)

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
