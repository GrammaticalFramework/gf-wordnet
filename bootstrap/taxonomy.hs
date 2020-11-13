import PGF2
import Data.Char
import Data.Maybe
import Data.Graph
import Data.Array((!))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Interval

main = do
  gr <- readPGF "build/ParseEng.pgf"
  absdefs <- fmap (Map.fromList . mapMaybe parseAbsSyn . lines) $ readFile "WordNet.gf"
  let Just eng = Map.lookup "ParseEng" (languages gr)
  xs1 <- fmap parseRecords $ readFile "data/dict/data.noun"
  xs2 <- fmap parseRecords $ readFile "data/dict/data.verb"
  xs3 <- fmap parseRecords $ readFile "data/dict/data.adv"
  xs4 <- fmap parseRecords $ readFile "data/dict/data.adj"
  let synsets  = xs1++xs2++xs3++xs4
      graph0   = Map.fromList synsets
      (xs,yss) = unzip (map (toEdges eng absdefs graph0) synsets)
      graph1   = Map.fromList xs
      graph2   = intervalLabeling graph1
      lex_graph= Map.fromListWith (++) (concat yss)
  writeFile "taxonomy.txt" (unlines ([unwords (src:show id:show interval:concat [[sym,show tgt] | (sym,tgt) <- links])++" | "++gloss | (src,(id,interval,links,gloss)) <- Map.toList graph2]++
                                     [unwords (src:concat [[sym,tgt] | (sym,tgt) <- links]) | (src,links) <- Map.toList lex_graph]
                                    ))


parseAbsSyn l =
  case words l of
    ("fun":fn:_) -> case break (=='\t') l of
                      (l1,'\t':l2) -> let (ds,l3) = splitDomains l2
                                      in Just (fn, (reverse . take 10 . reverse) l1)
                      _            -> Nothing
    _            -> Nothing
  where
    splitDomains ('[':cs) = split cs
      where
        trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

        split cs =
          case break (flip elem ",]") cs of
            (x,',':cs) -> let (xs,cs') = split (dropWhile isSpace cs)
                          in (trim x : xs, dropWhile isSpace cs')
            (x,']':cs) -> let x' = trim x
                          in (if null x' then [] else [x'], dropWhile isSpace cs)
            _          -> ([],       cs)
    splitDomains cs = ([],cs)


parseRecords = map toLinks . map words . drop 29 . lines
  where
    toLinks (offset:_:pos:w_cnt_s:ws) =
      let w_cnt                = read ('0':'x':w_cnt_s) :: Int
          (lemmas,p_cnt_s:ws1) = takeLemmas w_cnt ws
          p_cnt                = read p_cnt_s :: Int
          (ptrs,          ws2) = takePtrs p_cnt ws1
          ("|":           ws3) = dropWhile (/= "|") ws2
      in (mkSynsetId offset pos,(lemmas,ptrs,gloss (unwords ws3)))

    takeLemmas 0       ws  = ([],ws)
    takeLemmas i (w:id:ws) = let (lemmas,ws') = takeLemmas (i-1) ws
                             in (decodeLemma w:lemmas,ws')

    decodeLemma = map (\c -> if c == '_' then ' ' else c)

    takePtrs 0                     ws  = ([],ws)
    takePtrs i (sym:offset:pos:ref:ws) = let (ptrs,ws') = takePtrs (i-1) ws
                                         in ((sym,mkSynsetId offset pos,ref):ptrs,ws')

    mkSynsetId offset pos
      | pos == "s" = offset++"-a"
      | otherwise  = offset++"-"++pos

    gloss []               = []
    gloss (';':' ':'"':cs) = []
    gloss (c          :cs) = c : gloss cs

toEdges eng absdefs graph0 (synset1_id,(lemmas,ptrs,gloss)) =
  let sem_node  = (synset1_id,([(sym,synset_id) | (sym,synset_id,"0000") <- ptrs],gloss))
      lex_nodes = [(src,[(sym,tgt)])
                      | (sym,synset2_id,ref) <- ptrs,
                        ref /= "0000",
                        src <- getSrc synset1_id ref,
                        tgt <- getTgt synset2_id ref]
  in (sem_node,lex_nodes)
  where
    getSrc synset_id ref =
      toAbsId synset_id (lemmas !! (read ('0':'x':s)-1))
      where
        s = take 2 ref

    getTgt synset_id ref =
      case Map.lookup synset_id graph0 of
        Just (lemmas,ptrs,_) -> toAbsId synset_id (lemmas !! (read ('0':'x':s)-1))
      where
        s = drop 2 ref

    toAbsId synset_id lemma =
      (Set.toList . Set.fromList)
         [f | (f,_,_) <- lookupMorpho eng lemma,
              Map.lookup f absdefs == Just synset_id]

intervalLabeling graph = labeled_graph
  where
    (_, labeled_graph) = addLabels [] Map.empty (topSort tbl)

    (tbl, f) = graphFromEdges'
                   [(info, key, selectLinks (fst info))
                         | (key, info) <- Map.toList graph]

    selectLinks links =
      [key | (sym,key) <- links, dep sym]

    dep "~"  = True
    dep "~i" = True
    dep _    = False

    addLabels ex0 graph []     = (ex0,graph)
    addLabels ex0 graph (v:vs) =
      case Map.lookup key graph of
        Just (_,ex',_,_) -> addLabels (union ex0 ex') graph  vs
        Nothing          -> addLabels (union ex0 ex') graph' vs
      where
        pre  = Map.size graph
        post = Map.size graph'
        ((links,gloss), key, _) = f v

        labeled_links =
          [(sym,id) | (sym,offset)    <- links,
                      Just (id,_,_,_) <- [Map.lookup offset labeled_graph]]

        (ex',graph') = addLabels []
                         (Map.insert key (pre,union [(pre,post)] ex',labeled_links,gloss) graph)
                         (tbl ! v)
