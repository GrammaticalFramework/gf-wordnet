import EM
import Matching
import Control.Exception
import System.IO
import System.Environment

-- main = training
main = do
  args <- getArgs
  case args of
    "train":_         -> training
    "annotate":lang:_ -> annotation lang
    _                 -> do putStrLn "Syntax: udsenser train"
                            putStrLn "        udsenser annotate <concr syntax>"

training =
  bracket (status "Grammar Loading ..." $ newEMState "../Parse.pgf") freeEMState $ \st -> do
    status "Unigram smoothing ..." $ setupUnigramSmoothing st 1
    status "Setup ranking ..." $ setupRankingCallbacks st ranking_callbacks
    status "Collecting data ..." $ do
      importTreebank st "ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-test.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-dev.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-test.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-dev.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-test.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-dev.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-test.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-dev.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-PUD/en_pud-ud-test.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-train.conllu" "ParseBul"
      importTreebank st "ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-test.conllu" "ParseBul"
      importTreebank st "ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-dev.conllu" "ParseBul"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-train.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-test.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-dev.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-train.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-test.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-dev.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-PUD/sv_pud-ud-test.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-FTB/fi_ftb-ud-dev.conllu" "ParseFin"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-FTB/fi_ftb-ud-test.conllu" "ParseFin"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-FTB/fi_ftb-ud-train.conllu" "ParseFin"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-PUD/fi_pud-ud-test.conllu" "ParseFin"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-TDT/fi_tdt-ud-dev.conllu" "ParseFin"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-TDT/fi_tdt-ud-test.conllu" "ParseFin"
      importTreebank st "ud-treebanks-v2.2/UD_Finnish-TDT/fi_tdt-ud-train.conllu" "ParseFin"
    getBigramCount  st >>= \c -> hPutStrLn stdout ("Bigrams:  "++show c)
    getUnigramCount st >>= \c -> hPutStrLn stdout ("Unigrams: "++show c)
    status "Estimation ..." $ em_loop st 0 0
    status "Dumping ..." $ dump st "../Parse.probs" "../Parse.bigram.probs"

annotation lang =
  bracket (status "Grammar Loading ..." $ newEMState "../Parse.pgf") freeEMState $ \st -> do
    status "Setup preserve trees ..." $ setupPreserveTrees st
    status "Bigram smoothing ..." $ setupBigramSmoothing st 0.002
    status "Setup ranking ..." $ setupRankingCallbacks st ranking_callbacks
    status "Load model ..." $ loadModel st "../Parse.bigram.probs"
    status "Import data ..." $ do
      importTreebank st "" lang
    status "Export data ..." $ exportAnnotatedTreebank st ""

status msg io = do
  hPutStr stderr msg
  hFlush stderr
  r <- io
  hPutStrLn stderr ""
  return r

ranking_callbacks =
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

em_loop st i last_corpus_prob = do
  corpus_prob <- step st
  hPutStr stdout ("\n"++show i++" "++show corpus_prob++" ("++show (last_corpus_prob-corpus_prob)++")")
  hFlush stdout
  if abs (last_corpus_prob - corpus_prob) < 1e-4
    then return ()
    else em_loop st (i+1) corpus_prob

