import EM
import Matching
import Control.Exception
import System.IO
import System.Environment
import System.FilePath

main = do
  args <- getArgs
  case args of
    (fpath:args) -> bracket (status "Grammar Loading ..." $ newEMState fpath) freeEMState $ \st ->
                      case args of
                        "train":args      -> training st args
                        "annotate":lang:_ -> annotation st (replaceExtension fpath "bigram.probs") lang
                        _                 -> help
    _                -> help

help = do
  putStrLn "Syntax: udsenser <grammar> train"
  putStrLn "        udsenser <grammar> annotate <concr syntax>"

training st args = do
  status "Unigram smoothing ..." $ setupUnigramSmoothing st 1
  status "Setup ranking ..." $ setupRankingCallbacks st ranking_callbacks
  status "Collecting data ..." $ importTreebanks args
  getBigramCount  st >>= \c -> hPutStrLn stdout ("Bigrams:  "++show c)
  getUnigramCount st >>= \c -> hPutStrLn stdout ("Unigrams: "++show c)
  status "Estimation ..." $ em_loop st 0 0
  status "Dumping ..." $ dump st "Parse.probs" "Parse.bigram.probs"
  where
    importTreebanks []          = return ()
    importTreebanks (lang:args) = do
      let (fpaths,rest) = break (==",") args
      mapM_ (importTreebank st lang) fpaths
      case rest of
        (",":args) -> importTreebanks args
        _          -> return ()

annotation st bigram_fpath lang = do
    status "Setup preserve trees ..." $ setupPreserveTrees st
    status "Bigram smoothing ..." $ setupBigramSmoothing st 0.002
    status "Setup ranking ..." $ setupRankingCallbacks st ranking_callbacks
    status "Load model ..." $ loadModel st bigram_fpath
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

