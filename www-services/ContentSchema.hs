{-# LANGUAGE DeriveDataTypeable #-}

module ContentSchema where

import PGF2
import Database.Daison
import Data.Data
import Data.List(nub)
import Interval

data Update
  = UpdateLexeme
     { user   :: String
     , lex_id :: Fun
     , lang   :: String
     , def    :: String
     }
  | UpdateExample
     { user    :: String
     , line_no :: Int
     , def     :: String
     }
   deriving (Data,Show)

updates :: Table Update
updates = table "updates"
            `withIndex` updates_lex_idx
            `withIndex` updates_ex_idx
            `withIndex` updates_usr

updates_lex_idx :: Index Update (String,Fun,String)
updates_lex_idx = maybeIndex updates "lex_idx" 
                             (\u -> case u of
                                      UpdateLexeme{user=user,lex_id=lex_id,lang=lang} -> Just (user,lex_id,lang)
                                      _                                               -> Nothing)

updates_ex_idx  :: Index Update (String,Int)
updates_ex_idx  = maybeIndex updates "ex_idx"
                             (\u -> case u of
                                      UpdateExample{user=user,line_no=line_no}        -> Just (user,line_no)
                                      _                                               -> Nothing)

updates_usr :: Index Update String
updates_usr = index updates "usr" user
