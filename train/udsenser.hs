import EM
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
  hPutStr stdout msg
  hFlush stdout
  r <- io
  hPutStrLn stdout ""
  return r

ranking_callbacks =
  [("ADJ", adj_ranking)
  ,("ADV", adv_ranking)
  ,("INTJ", intj_ranking)
  ,("NOUN", noun_ranking)
  ,("PROPN", propn_ranking)
  ,("VERB", verb_ranking)
  ,("ADP", adp_ranking)
  ,("AUX", aux_ranking)
  ,("CCONJ", cconj_ranking)
  ,("DET", det_ranking)
  ,("PRON", pron_ranking)
  ,("SCONJ", sconj_ranking)
  ]

adj_ranking tree choice cat
  | cat == "A"     = 1
  | cat == "A2"    = 1
  | cat == "ACard" = 1
  | otherwise      = 0

adv_ranking tree choice cat
  | cat == "Adv" = 1
  | cat == "AdV" = 1
  | cat == "AdA" = 1
  | cat == "AdN" = 1
  | otherwise    = 0

intj_ranking tree choice cat
  | cat == "Interj" = 1
  | otherwise       = 0

noun_ranking tree choice cat
  | cat == "N"  = 1
  | cat == "N2" = 1
  | otherwise   = 0

propn_ranking tree choice cat
  | cat == "PN" = 1
  | otherwise   = 0

verb_ranking tree choice cat
  | cat == "V0"  = 1
  | cat == "V"   = 1
  | cat == "V2"  = 1
  | cat == "V3"  = 1
  | cat == "VA"  = 1
  | cat == "VS"  = 1
  | cat == "VQ"  = 1
  | cat == "VV"  = 1
  | cat == "V2A" = 1
  | cat == "V2S" = 1
  | cat == "V2Q" = 1
  | cat == "V2V" = 1
  | otherwise    = 0

adp_ranking tree choice cat
  | cat == "Prep"= 1
  | otherwise    = 0

aux_ranking tree choice cat
  | cat == "VV"  = 1
  | otherwise    = 0

cconj_ranking tree choice cat
  | cat == "Conj" = 1
  | otherwise     = 0

det_ranking tree choice cat
  | cat == "Det"   = 1
  | cat == "Quant" = 1
  | otherwise      = 0

pron_ranking tree choice cat
  | cat == "Pron"  = 1
  | otherwise      = 0

sconj_ranking tree choice cat
  | cat == "Subj"  = 1
  | otherwise      = 0

em_loop st i last_corpus_prob = do
  corpus_prob <- step st
  hPutStr stdout ("\n"++show i++" "++show corpus_prob++" ("++show (last_corpus_prob-corpus_prob)++")")
  hFlush stdout
  if abs (last_corpus_prob - corpus_prob) < 1e-4
    then return ()
    else em_loop st (i+1) corpus_prob
