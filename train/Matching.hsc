{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Matching( Fields(..),DepTree(..),Prop,Stat(..),
                 category, node, label, pos, equal_choice, best,
                 default_ranking_callbacks
               ) where

import PGF2
import Foreign
import Foreign.C

#include "em_core.h"

newtype Fields  = Fields  (Ptr ())
newtype DepTree = DepTree (Ptr ()) deriving Storable
data Stat = S {-# UNPACK #-} !CInt {-# UNPACK #-} !CInt
newtype Prop = Prop (CString -> Fields -> DepTree -> IO Stat)

category :: Cat -> [Prop] -> (Cat,CString -> Fields -> DepTree -> IO Stat)
category cat props = (cat,check props)
  where
    check []          = \choice fields dtree -> stat 0 1
    check (Prop p:ps) = \choice fields dtree -> do (S res1 c1) <- p choice fields dtree
                                                   (S res2 c2) <- check ps choice fields dtree
                                                   stat (res1+res2) (c1+c2)

node :: [Prop] -> Prop
node props = Prop $ \lemma fields (DepTree ptr) -> do
  n_children <- (#peek DepTree, n_children) ptr
  let children = ptr `plusPtr` (#offset DepTree, children)
  match lemma fields 0 n_children children
  where
    match lemma fields i n children
      | i >= n    = stat 0 1
      | otherwise = do ptr   <- peek (children `plusPtr` (i*sizeOf children))
                       stat1 <- check props lemma fields ptr
                       stat2 <- match lemma fields (i+1) n children
                       best stat1 stat2

    best s1@(S res1 _) s2@(S res2 _)
      | res1 > res2 = return $! s1
      | otherwise   = return $! s2

    check []          = \lemma fields ptr -> stat 0 1
    check (Prop p:ps) = \lemma fields ptr -> do (S res1 c1) <- p lemma fields (DepTree ptr)
                                                (S res2 c2) <- check ps lemma fields ptr
                                                stat (res1+res2) (c1+c2)

label :: String -> Prop
label l = Prop $ \lemma fields dtree  -> do
  res <- withCString l (dtree_match_label fields dtree)
  stat res 1

foreign import ccall unsafe dtree_match_label :: Fields -> DepTree -> CString -> IO CInt

pos :: String -> Prop
pos l = Prop $ \lemma fields dtree -> do
  res <- withCString l (dtree_match_pos fields dtree)
  stat res 1

foreign import ccall unsafe dtree_match_pos :: Fields -> DepTree -> CString -> IO CInt

equal_choice :: Prop
equal_choice = Prop $ \lemma fields dtree  -> do
  res <- dtree_match_same_lemma fields dtree lemma
  stat res 1

foreign import ccall unsafe dtree_match_same_lemma :: Fields -> DepTree -> CString -> IO CInt

stat res c = return $! S res c

best :: [Prop] -> Prop
best = foldl1 best
  where
    best (Prop f) (Prop g) = Prop $ \lemma fields dtree -> do
      s1@(S res1 _) <- f lemma fields dtree
      s2@(S res2 _) <- g lemma fields dtree
      if res1 > res2
        then return $ s1
        else return $ s2


default_ranking_callbacks =
  [ category "A"     [pos "ADJ"]
  , category "A2"    [pos "ADJ"
                     ,node [label "obl"
                           ,node [label "case"
                                 ,equal_choice
                                 ]
                           ]
                     ]
  , category "ACard" [pos "ADJ"]
  , category "Adv"   [pos "ADV"]
  , category "AdV"   [pos "ADV"]
  , category "AdA"   [pos "ADV"]
  , category "AdN"   [pos "ADV"]
  , category "Interj"[pos "INTJ"]
  , category "N"     [pos "NOUN"]
  , category "N2"    [pos "NOUN"
                     ,node [label "nmod"
                           ,node [label "case"
                                 ,equal_choice
                                 ]
                           ]
                     ]
  , category "PN"    [pos "PROPN"]
  , category "V0"    [pos "VERB"]
  , category "V"     [pos "VERB"
                     ,has_part
                     ]
  , category "V2"    [pos "VERB"
                     ,best [has_dir_obj
                           ,has_prep_arg
                           ]
                     ,has_part
                     ]
  , category "V3"    [pos "VERB"
                     ,node [has_dir_obj
                           ]
                     ,best [has_indir_obj
                           ,has_prep_arg
                           ]
                     ,has_part
                     ]
  , category "VA"    [pos "VERB"
                     ,has_adj_arg
                     ,has_part
                     ]
  , category "VS"    [pos "VERB"
                     ,has_sent_arg
                     ,has_part
                     ]
  , category "VQ"    [pos "VERB"
                     ,has_quest_arg
                     ,has_part
                     ]
  , category "VV"    [best [pos "VERB"
                           ,pos "AUX"
                           ]
                     ,has_verb_arg
                     ,has_part
                     ]
  , category "V2A"   [pos "VERB"
                     ,has_dir_obj
                     ,has_adj_arg
                     ,has_part
                     ]
  , category "V2S"   [pos "VERB"
                     ,has_dir_obj
                     ,has_sent_arg
                     ,has_part
                     ]
  , category "V2Q"   [pos "VERB"
                     ,has_dir_obj
                     ,has_quest_arg
                     ,has_part
                     ]
  , category "V2V"   [pos "VERB"
                     ,has_dir_obj
                     ,has_verb_arg
                     ,has_part
                     ]
  , category "Prep"  [best [pos "ADP"
                           ,pos "SCONJ"
                           ]
                     ]
  , category "Conj"  [pos "CCONJ"]
  , category "Det"   [pos "DET"]
  , category "Quant" [pos "DET"]
  , category "Pron"  [pos "PRON"]
  , category "Subj"  [pos "SCONJ"]
  ]
  where
    has_dir_obj =
      node [best [label "obj"
                 ,label "nsubj:pass"
                 ]
           ]

    has_indir_obj =
      node [best [label "iobj"
                 ,label "nsubj:pass"
                 ]
           ]

    has_prep_arg =
      node [label "obl"
           ,pos "NOUN"
           ,node [label "case"
                 ,pos "PREP"
                 ,equal_choice
                 ]
           ]

    has_adj_arg =
      node [label "xcomp"
           ,pos "ADJ"
           ]

    has_sent_arg =
      node [label "xcomp"
           ,pos "VERB"
           ,node [best [label "nsubj"
                       ,label "nsubj:pass"
                       ]
                 ]
           ]

    has_quest_arg =
      node [label "xcomp"
           ,pos "VERB"
           ]

    has_verb_arg =
      node [label "xcomp"
           ,pos "VERB"
           ]

    has_part = 
      node [label "compound:prt"
           ,equal_choice
           ]
