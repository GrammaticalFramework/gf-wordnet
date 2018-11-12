import EM
import Matching
import GF2UED
import PGF2
import Control.Exception
import System.IO
import System.Environment
import System.FilePath

main = do
  args <- getArgs
  case args of
    (fpath:args) -> bracket (status "Grammar Loading ..." $ newEMState fpath) freeEMState $ \st ->
                      case args of
                        "train":args      -> training st "Parse.labels" args
                        "annotate":lang:_ -> annotation st (replaceExtension fpath "bigram.probs") lang
                        _                 -> help
    _                -> help

help = do
  putStrLn "Syntax: udsenser <grammar> train"
  putStrLn "        udsenser <grammar> annotate <concr syntax>"

training st labels_fpath args = do
  status "Unigram smoothing ..." $ setupUnigramSmoothing st 1
  status "Setup ranking ..." $ setupRankingCallbacks st default_ranking_callbacks
  status "Collecting data ..." $ (readDepConfig labels_fpath >>= \config -> importTreebanks config args)
  getBigramCount  st >>= \c -> hPutStrLn stdout ("Bigrams:  "++show c)
  getUnigramCount st >>= \c -> hPutStrLn stdout ("Unigrams: "++show c)
  status "Estimation ..." $ em_loop st 0 0
  status "Dumping ..." $ dump st "Parse.probs" "Parse.bigram.probs"
  where
    importTreebanks config []          = return ()
    importTreebanks config (lang:args) = do
      let (fpaths,rest) = break (==",") args
      if lang == "abstract"
        then mapM_ (importExamples config st) fpaths
        else mapM_ (importTreebank st lang) fpaths
      case rest of
        (",":args) -> importTreebanks config args
        _          -> return ()
        
    importExamples config st fpath = do
      ls <- fmap lines $ readFile fpath
      sequence_ [addDepTree st dtree
                      | l <- ls,
                        take 4 l == "abs:",
                        Just e <- [readExpr (drop 4 l)],
                        dtree <- expr2DepForest config e
                      ]

annotation st bigram_fpath lang = do
  status "Setup preserve trees ..." $ setupPreserveTrees st
  status "Bigram smoothing ..." $ setupBigramSmoothing st 0.002
  status "Setup ranking ..." $ setupRankingCallbacks st default_ranking_callbacks
  status "Load model ..." $ loadModel st bigram_fpath
  status "Import data ..." $ do
    importTreebank st lang ""
  status "Export data ..." $ exportAnnotatedTreebank st ""

status msg io = do
  hPutStr stderr msg
  hFlush stderr
  r <- io
  hPutStrLn stderr ""
  return r

em_loop st i last_corpus_prob = do
  corpus_prob <- step st
  hPutStr stdout ("\n"++show i++" "++show corpus_prob++" ("++show (last_corpus_prob-corpus_prob)++")")
  hFlush stdout
  if abs (last_corpus_prob - corpus_prob) < 1e-4
    then return ()
    else em_loop st (i+1) corpus_prob

