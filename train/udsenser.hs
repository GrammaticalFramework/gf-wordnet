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
      importTreebank st "ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-train.conllu" "ParseEng"
      importTreebank st "ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-train.conllu" "ParseBul"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-train.conllu" "ParseSwe"
      importTreebank st "ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-train.conllu" "ParseSwe"
    getBigramCount  st >>= \c -> hPutStrLn stdout ("Bigrams:  "++show c)
    getUnigramCount st >>= \c -> hPutStrLn stdout ("Unigrams: "++show c)
    status "Estimation ..." $ em_loop st 0 0
    status "Dumping ..." $ dump st "../Parse.probs" "../Parse.bigram.probs"

annotation lang =
  bracket (status "Grammar Loading ..." $ newEMState "../Parse.pgf") freeEMState $ \st -> do
    status "Setup preserve trees ..." $ setupPreserveTrees st
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
  , category "A2"    [pos "ADJ"]
  , category "ACard" [pos "ADJ"]
  , category "Adv"   [pos "ADV"]
  , category "AdV"   [pos "ADV"]
  , category "AdA"   [pos "ADV"]
  , category "AdN"   [pos "ADV"]
  , category "Interj"[pos "INTJ"]
  , category "N"     [pos "NOUN"]
  , category "N2"    [pos "NOUN"]
  , category "PN"    [pos "PROPN"]
  , category "V0"    [pos "VERB"]
  , category "V"     [pos "VERB"]
  , category "V2"    [pos "VERB"
                     ,node [best (label "obj")
                                 (label "nsubj:pass")
                           ]
                     ]
  , category "V3"    [pos "VERB"
                     ,node [best (label "obj")
                                 (label "nsubj:pass")
                           ]
                     ,node [best (label "iobj")
                                 (label "nsubj:pass")
                           ]
                     ]
  , category "VA"    [pos "VERB"]
  , category "VS"    [pos "VERB"]
  , category "VQ"    [pos "VERB"]
  , category "VV"    [best (pos "VERB") (pos "AUX")]
  , category "V2A"   [pos "VERB"]
  , category "V2S"   [pos "VERB"]
  , category "V2Q"   [pos "VERB"]
  , category "V2V"   [pos "VERB"]
  , category "Prep"  [pos "ADP"]
  , category "Conj"  [pos "CCONJ"]
  , category "Det"   [pos "DET"]
  , category "Quant" [pos "DET"]
  , category "Pron"  [pos "PRON"]
  , category "Subj"  [pos "SCONJ"]
  ]

em_loop st i last_corpus_prob = do
  corpus_prob <- step st
  hPutStr stdout ("\n"++show i++" "++show corpus_prob++" ("++show (last_corpus_prob-corpus_prob)++")")
  hFlush stdout
  if abs (last_corpus_prob - corpus_prob) < 1e-4
    then return ()
    else em_loop st (i+1) corpus_prob
