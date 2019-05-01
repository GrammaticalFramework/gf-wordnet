import Text.EditDistance -- pkg edit-distance
import Data.List
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe(maybe,mapMaybe,fromMaybe)
import Data.Char
import Data.IORef
import Debug.Trace
import System.Random
import System.Random.Shuffle -- pkg random-shuffle
import System.Environment
import Control.Monad(forM_)
import PGF2
import Transliteration
import Morpho

{- HOW TO USE IT
   ~~~~~~~~~~~~~
   You need data from the Open Multilingual WordNet for 
   the languages that you are interested in.  The data
   files for each language should be stored in `data/wn-data-<language
   code>.tab´. Run the command:
   
   runghc bootstrap/bootstrap.hs <language code>
   
   This will create two files `predictions.tsv` which lists the choices
   made by the script and a draft of the new concrete syntax for WordNet.
   The script will also look for morphology definitions in:
   
   lib/src/<language code>*/Dict???.gf
   lib/src/<language code>*/Irreg???.gf
-}

train = False
useTenFold = False

main = do
  [lang] <- getArgs
  let lang' = case lang of
                ""     -> ""
                (c:cs) -> toUpper c:cs
  gr         <- readPGF "build/Parse.noprobs.pgf"
  (src,funs) <- fmap (toGFEntries gr)           $ readFile "WordNet.gf"
  wn30v31    <- fmap toMapping                  $ readFile "bootstrap/wn30map31.txt"
  transl     <- readTransliteration "bootstrap/translit.txt"
  dst        <- fmap (toWNEntries lang wn30v31) $ readFile ("data/wn-data-"++lang++".tab")
  morpho     <- readMorpho lang

  let wps = addCounts transl (join src dst)

  let features = addFeatures wps
  let predictions = classify features

  writeFile "predictions.tsv" (unlines [untsv [sense_id,
                                               lemma1,
                                               lemma2,
                                               show c,show d,
                                               show crank,show drank,
                                               show pred]
                                          | (sense_id,lemma1,lemma2,c,d,crank,drank,pred) <- predictions])
  let dict = Map.fromListWith (++) [(lex_id,[lemma]) | (_,lex_id,lemma,_,_,_,_,True) <- predictions]

  writeFile ("WordNet"++lang'++".gf") (unlines
      (["concrete WordNet"++lang'++" of WordNet = Cat"++lang'++" ** open Construction"++lang'++", Grammar"++lang'++", Paradigms"++lang'++", BeschCat, MorphoCat, (S=StructuralCat), Prelude in {"] ++
       [""]++
       ["lin "++lex_id++" = "++prediction2gf "unchecked" morpho dict lex_id | lex_id <- funs]++
       ["}"]))

toGFEntries gr s =
  let ls = (concatMap parseLine . lines) s
      senses = Map.fromListWith (++) [(synset_id,[(fn,lins)]) | (Just synset_id,fn,lins) <- ls]
      funs   = [fn | (_,fn,_) <- ls]
  in (senses,funs)
  where
    cncs = Map.elems (languages gr)

    parseLine l =
      case words l of
        ("fun":fn:_) -> case break (=='\t') l of
                          (l1,'\t':l2) -> let synset_id = (reverse . take 10 . reverse) l1
                                          in [(Just synset_id,fn,map (funLins fn) cncs)]
                          _            -> [(Nothing,fn,[])]
        _            -> []

    funLins fn cnc
      | arity fn == 0 && hasLinearization cnc fn = linearizeAll cnc (mkApp fn [])
      | otherwise                                = []

    arity fn = maybe 0 (\(hs,_,_) -> length hs) (fmap unType (functionType gr fn))

toMapping =
  Map.fromList .
  map (toTuple . tsv) .
  dropWhile (\l -> take 1 l == "#") .
  lines
  where
    toTuple [tag,wn30,wn31] = (wn30++"-"++tag,wn31++"-"++tag)

toWNEntries lng wn30v31 =
  foldr addElem Map.empty .
  mapMaybe (toEntry . tsv) .
  tail .
  lines
  where
    toEntry [sense_id,rel,w]
      | rel == "lemma" || rel == lng++":lemma" = fmap (flip (,) w) (Map.lookup sense_id wn30v31)
    toEntry _                                  = Nothing

    addElem (k,a) =
      Map.alter (\mb_as -> Just (a:fromMaybe [] mb_as)) k

join src dst =
  [(sense_id,x,y) | (sense_id,xs) <- Map.toList src,
                    x <- xs,
                    y <- fromMaybe [] (Map.lookup sense_id dst)]

addCounts transl src_dst =
  [(sense_id,x,y,c,d) | (sense_id,(x,linss),y) <- src_dst, let (c,d) = counts linss y]
  where
    cmap  = Map.fromListWith (+) [((idx,lin,y),1) | (sense_id,(x,linss),y) <- src_dst, (idx,lins) <- zip [0..] linss, lin <- lins]
    cdmap = Map.mapWithKey (\(idx,lin,y) c -> (c,dist lin y)) cmap

    counts linss y = 
      let (cs,ds) = unzip [fromMaybe (0,0) (Map.lookup (idx,lin,y) cdmap) | (idx,lins) <- zip [0..] linss, lin <- lins]
      in (sum cs,minimum (maxBound:ds))

    dist x y = levenshteinDistance defaultEditCosts (transl x) (transl y)

transliterate transl cs =
  concat [fromMaybe [c] (Map.lookup c transl) | c <- cs]

toReferencePair (fis:fi:ens:en:_) = (conv ens,en,fi)
  where
    conv s = drop 8 s ++ ['-',s !! 7]

toAlignmentPair (eng:fin:_:prob:_) = ((mapEng eng,mapFin fin),read prob :: Double)
  where
    mapEng w =
      init w ++ ((:[]) $
      case last w of
        'n' -> 'n'
        'v' -> 'v'
        'j' -> 'a'
        'r' -> 'r'
        c   -> c)

    mapFin w =
      init w ++ ((:[]) $
      case last w of
        'n' -> 'n'
        'v' -> 'v'
        'j' -> 'a'
        'a' -> 'a'
        'r' -> 'r'
        c   -> c)

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

untsv :: [String] -> String
untsv = intercalate "\t"

addFeatures ps =
  let (xs,ys)   = takeSynset ps
      (cds,xs') = mapAccumL (addValues cds) (Set.empty,Set.empty) xs
  in if null xs
       then []
       else xs' ++ addFeatures ys
  where
    takeSynset []     = ([],[])
    takeSynset (p:ps) = let sense_id = get_sense_id p
                            (ps1,ps2) = break (\p1 -> get_sense_id p1 /= sense_id) ps
                        in (p : ps1, ps2)
      where
         get_sense_id (sense_id,_,_,_,_) = sense_id

    addValues cds (cs,ds) (sense_id,lemma1,lemma2,c,d) =
      let cs' = Set.insert c cs
          ds' = Set.insert d ds
          crank = findIndex 1 c (Set.toDescList (fst cds))
          drank = findIndex 1 d (Set.toAscList  (snd cds))
      in ((cs',ds'),(sense_id,lemma1,lemma2,c,d,crank,drank))
      where
        findIndex i x []     = i
        findIndex i x (y:ys)
          | x == y           = i
          | otherwise        = findIndex (i+1) x ys

tenfold gen ps =
  let synsets = takeSynset ps
      len     = length synsets
      len10   = len `div` 10
  in splitData len10 [] (shuffle' synsets len gen)
  where
    takeSynset []     = []
    takeSynset (p:ps) = let sense_id  = get_sense_id p
                            (ps1,ps2) = break (\p1 -> get_sense_id p1 /= sense_id) ps
                        in (p : ps1) : takeSynset ps2
      where
        get_sense_id (sense_id,_,_,_,_,_,_,_) = sense_id

    splitData len10 zs ps =
          let (xs,ys) = splitAt len10 ps
          in if null ys
               then []
               else (concat xs,concat (zs++ys)) : splitData len10 (xs++zs) ys

classify ps =
  let (xs,ys)    = takeSynset ps
      xs'        = sortBy descProb (map pairProb xs)
      (ids,sel1) = pick1 ([],[]) xs'
      sel2       = pick2 ids xs'
      sel        = sel1++sel2
  in if null xs
       then []
       else map (annotate sel) xs ++ classify ys
  where
    takeSynset []     = ([],[])
    takeSynset (p:ps) = let sense_id = get_sense_id p
                            (ps1,ps2) = break (\p1 -> get_sense_id p1 /= sense_id) ps
                        in (p : ps1, ps2)
      where
        get_sense_id (sense_id,_,_,_,_,_,_) = sense_id

    pairProb x@(sense_id,lemma1,lemma2,c,d,crank,drank) =
      (lemma1,lemma2,exp(-fromIntegral (crank+drank)))

    descProb (_,_,p1) (_,_,p2) = compare p2 p1

    pick1 ids             []                       = (ids,[])
    pick1 ids@(ids1,ids2) ((lemma1,lemma2,prob):xs)
      | not (elem lemma1 ids1 || elem lemma2 ids2) = let (ids',xs') = pick1 (lemma1:ids1,lemma2:ids2) xs
                                                     in (ids',(lemma1,lemma2) : xs')
      | otherwise                                  = pick1 ids xs

    pick2 ids             []   = []
    pick2 ids@(ids1,ids2) ((lemma1,lemma2,prob):xs)
      | not (elem lemma1 ids1) = (lemma1,lemma2) : pick2 (lemma1:ids1,lemma2:ids2) xs
   -- | not (elem lemma2 ids2) = (lemma1,lemma2) : pick2 (lemma1:ids1,lemma2:ids2) xs
      | otherwise              = pick2 ids xs

    annotate sel (sense_id,lemma1,lemma2,c,d,crank,drank) =
      (sense_id,lemma1,lemma2,c,d,crank,drank,elem (lemma1,lemma2) sel)

randomChoice g ps =
  let (xs,ys)  = takeSynset ps
      (g',xs') = mapAccumL pairProb g xs
      sel      = pick [] (sortBy descProb xs')
  in if null xs
       then []
       else map (annotate sel) xs ++ randomChoice g' ys
  where
    takeSynset []     = ([],[])
    takeSynset (p:ps) = let sense_id = get_sense_id p
                            (ps1,ps2) = break (\p1 -> get_sense_id p1 /= sense_id) ps
                        in (p : ps1, ps2)
      where
            get_sense_id (sense_id,_,_,_,_,_,_,_) = sense_id

    pairProb g x@(sense_id,lemma1,lemma2,c,d,crank,drank,cls) =
      let (prob,g') = randomR (0.0,1.0::Double) g
      in (g',(lemma1,lemma2,prob))

    descProb (_,_,p1) (_,_,p2) = compare p2 p1

    pick ids []                  = []
    pick ids ((lemma1,lemma2,prob):xs)
      | not (elem lemma1 ids) = (lemma1,lemma2) : pick (lemma1:lemma2:ids) xs
      | not (elem lemma2 ids) = (lemma1,lemma2) : pick (lemma1:lemma2:ids) xs
      | otherwise                = pick ids xs

    annotate sel (sense_id,lemma1,lemma2,c,d,crank,drank,cls) =
      (sense_id,lemma1,lemma2,c,d,crank,drank,cls,elem (lemma1,lemma2) sel)


alignmentChoice e2f ps =
  let (xs,ys) = takeSynset ps
      xs'     = map pairProb xs
      sel     = pick ([],[]) (sortBy descProb xs')
  in if null xs
       then []
       else map (annotate sel) xs ++ alignmentChoice e2f ys
  where
    takeSynset []     = ([],[])
    takeSynset (p:ps) = let sense_id = get_sense_id p
                            (ps1,ps2) = break (\p1 -> get_sense_id p1 /= sense_id) ps
                        in (p : ps1, ps2)
      where
            get_sense_id (sense_id,_,_,_,_,_,_,_) = sense_id

    pairProb x@(sense_id,lemma1,lemma2,c,d,crank,drank,cls) =
      let prob = fromMaybe 0 (Map.lookup (lemma1++"|"++[sense_id!!9],lemma2++"|"++[sense_id!!9]) e2f)
      in (lemma1,lemma2,prob)

    descProb (_,_,p1) (_,_,p2) = compare p2 p1

    pick ids             []                  = []
    pick ids@(ids1,ids2) ((lemma1,lemma2,prob):xs)
      | not (elem lemma1 ids1) = (lemma1,lemma2) : pick (lemma1:ids1,lemma2:ids2) xs
      | not (elem lemma2 ids2) = (lemma1,lemma2) : pick (lemma1:ids1,lemma2:ids2) xs
      | otherwise              = pick ids xs

    annotate sel (sense_id,lemma1,lemma2,c,d,crank,drank,cls) =
      (sense_id,lemma1,lemma2,c,d,crank,drank,cls,elem (lemma1,lemma2) sel)

