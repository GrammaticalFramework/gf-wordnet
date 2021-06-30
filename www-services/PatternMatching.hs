{-# LANGUAGE MonadComprehensions #-}
module PatternMatching(matchPattern,Variable,Triple,Pattern(..),Binding(..)) where

import Database.Daison
import PGF2
import SenseSchema
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (maybe)
import Control.Monad
import Text.JSON

type Variable = String
type Triple   = (Variable,PointerSymbol,Variable)
data Pattern  = Pattern [Triple] [(Variable,Fun)] deriving Show

data Binding
  = LexemeValue (Key Lexeme) Lexeme (Maybe Synset)
  | SynsetValue (Key Synset) Synset [(Key Lexeme,Lexeme)]
  deriving Show

type Env = Map.Map Variable Binding

instance JSON Pattern where
  readJSON (JSObject jsobj) = do
    triples <- fmap (map (\(s,p,o) -> (s,read p,o))) (valFromObj "triples" jsobj)
    jenv    <- valFromObj "env" jsobj
    env <- decJSDict "env" jenv
    return (Pattern triples env)
  readJSON _ = fail "JSON object expected"
    
matchPattern (Pattern ts values) = do
  env <- initEnv values
  matchTriples ts env
  where
    initEnv []                     = return Map.empty
    initEnv ((var,lex_fun):values) = do
      res <- select [LexemeValue key lexeme mb_synset
                        | (key,lexeme) <- fromIndex lexemes_fun (at lex_fun)
                        , mb_synset <- maybe (return Nothing)
                                             (\key -> fmap Just $ from synsets (at key))
                                             (synset lexeme)
                        ]
      case res of
        []        -> mzero
        (value:_) -> do env <- initEnv values
                        return (Map.insert var value env)

    matchTriples []           env = return env
    matchTriples ((x,p,y):ts) env = do
      env <- matchTriple x p y (maybe (fullScan x p y) (\p -> matchTriple y p x (fullScan x p y)) (inversePointer p)) env
      matchTriples ts env

    matchTriple x p y cont env
      | all isDigit x = do
            let key = read x
            synset <- from synsets (at key)
            (followSynsetPtr synset p y env
             `mplus`
             do lexemes <- select (fromIndex lexemes_synset (at key))
                (id,lexeme) <- anyOf lexemes
                followLexemePtr lexeme p y env)
      | otherwise     =
           case Map.lookup x env of
             Just (LexemeValue _ lexeme mb_syn)  -> followLexemePtr lexeme p y env
                                                    `mplus`
                                                    case mb_syn of
                                                      Just synset -> followSynsetPtr synset p y env
                                                      Nothing     -> mzero
             Just (SynsetValue _ synset lexemes) -> followSynsetPtr synset p y env
                                                    `mplus`
                                                    do (id,lexeme) <- anyOf lexemes
                                                       followLexemePtr lexeme p y env
             Nothing                             -> cont env

    fullScan x p y env = do
      (key,lexeme) <- from lexemes everything
      mb_syn       <- case synset lexeme of
                        Just synset_id -> fmap Just (from synsets (at synset_id))
                        Nothing        -> return Nothing
      (let env' = Map.insert x (LexemeValue key lexeme mb_syn) env
       in followLexemePtr lexeme p y env'
          `mplus`
          case mb_syn of
            Just synset -> followSynsetPtr synset p y env'
            Nothing     -> mzero)

    followLexemePtr lexeme p y env = do
      (q,key) <- anyOf (lex_pointers lexeme)
      guard (q == p)
      bindLexeme y key env

    followSynsetPtr synset p y env = do
      (q,key) <- anyOf (pointers synset)
      guard (q == p)
      bindSynset y key env

    bindLexeme y key env = do
      case Map.lookup y env of
        Just (LexemeValue key' _ _)     -> do
          guard (key == key')
          return env
        Just (SynsetValue key' synset' lexemes) -> do
          case lookup key lexemes of
            Just lexeme -> return (Map.insert y (LexemeValue key lexeme (Just synset')) env)
            Nothing     -> mzero
        Nothing                         -> do
          lexeme    <- from lexemes (at key)
          mb_synset <- case synset lexeme of
                         Nothing        -> return Nothing
                         Just synset_id -> do synset <- from synsets (at synset_id)
                                              return (Just synset)
          return (Map.insert y (LexemeValue key lexeme mb_synset) env)

    bindSynset y key env = do
      case Map.lookup y env of
        Just (LexemeValue _ lexeme _)     -> do
          guard (Just key == synset lexeme)
          return env
        Just (SynsetValue key' _ lexemes) -> do
          guard (key == key')
          return env
        Nothing                           -> do
          synset <- from synsets (at key)
          lexemes <- select (fromIndex lexemes_synset (at key))
          return (Map.insert y (SynsetValue key synset lexemes) env)
