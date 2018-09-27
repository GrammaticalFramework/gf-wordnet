module CONLLReader(readDepTrees,depTreeUnigrams,depTreeBigrams) where

import PGF2
import Data.Tree
import Data.List
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map

data POS = ADJ | ADP | ADV | 
           AUX | CCONJ | DET | 
           INTJ | NOUN | NUM | 
           PART | PRON | PROPN | 
           PUNCT | SCONJ | SYM | 
           VERB | X
         deriving (Eq,Read,Show)

data Label =
    L_wildcard   | L_acl       | L_advcl
  | L_advmod     | L_amod      | L_appos
  | L_aux        | L_case      | L_cc
  | L_ccomp      | L_compound  | L_conj
  | L_cop        | L_csubj     | L_dep
  | L_det        | L_discourse | L_dislocated
  | L_expl       | L_fixed     | L_flat
  | L_goeswith   | L_iobj      | L_list
  | L_mark       | L_nmod      | L_nsubj
  | L_nummod     | L_obj       | L_obl
  | L_orphan     | L_parataxis | L_punct
  | L_reparandum | L_root      | L_vocative
  | L_xcomp
  deriving (Eq,Show)

data DepNode = DepNode {
  word   :: String,
  pos    :: POS,
  label  :: Label,
  morpho :: [(Fun,String,Float)]
  } deriving Show

type DepTree = Tree DepNode

readDepTrees :: Concr -> [String] -> [DepTree]
readDepTrees cnc = map filterDepTree . readDepTrees Map.empty
  where
    readDepTrees intern []     = []
    readDepTrees intern (l:ls)
      | take 1 l == "#" = readDepTrees intern ls
      | otherwise       = let (ls1,ls2) = break null ls
                              (intern', tree) = conll2deptree intern (map tsv (l:ls1))
                          in tree :
                             case ls2 of
                               []    -> []
                               _:ls2 -> readDepTrees intern' ls2

    conll2deptree intern wss = (intern', head (children "0"))
      where
        (intern',dns) = mapAccumL line2depnode intern wss
        children r    = [Node dn (children i) | (i,dn,j) <- dns, j == r]

        line2depnode intern ws =
          let lbl = case Map.lookup (takeWhile (/=':') (ws !! 7)) labelNames of
                      Just lbl -> lbl
                      Nothing  -> error (ws !! 7)
              (dn,intern') = depNode intern (ws !! 1) (read (ws !! 3)) lbl
          in (intern', (ws !! 0, dn, ws !! 6))

        -- intern is used to internalize word forms and the lookup results when
        -- the same word form appear multiple times.
        depNode intern word pos lbl =
          case Map.lookup word intern of
            Just (word,an) -> (DepNode word pos lbl an, intern)
            Nothing        -> let an = lookupMorpho cnc word
                              in (DepNode word pos lbl an, Map.insert word (word,an) intern)

labelNames =
  Map.fromList 
    [("_", L_wildcard),          ("acl", L_acl)
    ,("advcl",L_advcl),          ("advmod",L_advmod)
    ,("amod",L_amod),            ("appos",L_appos)
    ,("aux",L_aux),              ("case",L_case)
    ,("cc",L_cc),                ("ccomp",L_ccomp)
    ,("compound",L_compound),    ("conj",L_conj)
    ,("cop",L_cop),              ("csubj",L_csubj)
    ,("dep",L_dep),              ("det",L_det)
    ,("discourse",L_discourse),  ("dislocated", L_dislocated)
    ,("expl",L_expl),            ("fixed",L_fixed)
    ,("flat",L_flat),            ("goeswith",L_goeswith)
    ,("iobj",L_iobj),            ("list",L_list)
    ,("mark",L_mark),            ("nmod",L_nmod)
    ,("nsubj",L_nsubj),          ("nummod",L_nummod)
    ,("obj",L_obj),              ("obl",L_obl)
    ,("orphan",L_orphan),        ("parataxis",L_parataxis)
    ,("punct",L_punct),          ("reparandum",L_reparandum)
    ,("root",L_root),            ("vocative",L_vocative)
    ,("xcomp",L_xcomp)
    ]

filterDepTree :: DepTree -> DepTree
filterDepTree =
  updateNode $
    promote (hasPOS NOUN) (hasCat "N" `or` hasCat "N2") `next`
    promote (hasPOS PROPN) (hasCat "PN") `next`
    promote (hasPOS ADJ)  (hasCat "A" `or` hasCat "A2") `next`
    stop (hasPOS AUX) (hasCat "N" `or`
                       hasCat "N2" `or`
                       hasCat "PN" `or`
                       hasCat "A" `or`
                       hasCat "A2" `or`
                       hasCat "V" `or`
                       hasCat "V2" `or`
                       hasCat "VA" `or`
                       hasCat "VS" `or`
                       hasCat "VQ" `or`                           
                       hasCat "V3" `or`                           
                       hasCat "V2A" `or`
                       hasCat "V2V" `or`
                       hasCat "V2S"  `or`
                       hasCat "V2Q") `next`
    promote (hasPOS VERB) (hasCat "V" `or`
                           hasCat "V2" `or`
                           hasCat "VA" `or`
                           hasCat "VV" `or`
                           hasCat "VS" `or`
                           hasCat "VQ" `or`
                           hasCat "V3" `or`
                           hasCat "V2A" `or`
                           hasCat "V2V" `or`
                           hasCat "V2S"  `or`
                           hasCat "V2Q") `next`
    promote (hasDep L_obj)  (hasCat "V2" `or`
                             hasCat "V2A" `or`
                             hasCat "V2V" `or`
                             hasCat "V2S"  `or`
                             hasCat "V2Q" `or`
                             hasCat "V3") `next`
    promote (hasDep L_iobj) (hasCat "V3") `next`
    checkPart `next`
    stop (hasPOS PART) (const True) `next`
    promote (hasPOS ADP) (hasCat "Prep")
  where
    hasDep lbl dn children = elem lbl [label (rootLabel c) | c <- children]
    hasPOS p   dn children = pos dn == p

    hasCat cat (f,_,_)  = (reverse . take (1+length cat) . reverse) f == '_':cat

    or f g x = f x || g x

    updateNode f (Node dn children) =
      let (children',morpho') = (select . f dn . annotate) (children,morpho dn)
      in Node dn{morpho=morpho'} (map filterDepTree children')
      where
        annotate (children,morpho) = (children,zip (repeat (0::Int)) morpho)
        select   (children,ms)     = let max = maximum (map fst ms)
                                     in (children,[m | (c,m) <- ms, c == max])

    stop f g dn st@(children,ms)
      | f dn children = (children,filter (not . g . snd) ms)
      | otherwise     = st

    demote f g dn st@(children,ms)
      | f dn children = let ms' = [if g m then (c-1,m) else (c,m) | (c,m) <- ms]
                        in (children,ms')
      | otherwise     = st

    promote f g dn st@(children,ms)
      | f dn children = let ms' = [if g m then (c+1,m) else (c,m) | (c,m) <- ms]
                        in (children,ms')
      | otherwise     = st

    checkPart dn st@(children,ms) = fromMaybe st (check children ms)
      where
        check []             ms  = Nothing
        check (dt@(Node dn' _):children) ms
          | pos dn' == ADP &&
            (not . null) part_ms = Just (children,part_ms)
          | otherwise = do (children,part_ms) <- check children ms
                           return (dt:children,part_ms)
          where
            part_ms = [m | m@(c,(f,_,_)) <- ms, elem f (map fst3 (morpho dn'))]

    next f g dn st = f dn (g dn st)

fst3 (x,_,_) = x

depTreeUnigrams (Node dn children)
  | null (morpho dn) = concatMap depTreeUnigrams children
  | otherwise        = (nub (map fst3 (morpho dn)),1) :
                       concatMap depTreeUnigrams children

depTreeBigrams (Node dn children) =
  [(cross,1) | Node dn' _ <- children,
               let cross = nub [(h,m) | (h,_,_) <- morpho dn,
                                        (m,_,_) <- morpho dn'],
               not (null cross)] ++
  concatMap depTreeBigrams children

tsv :: String -> [String]
tsv "" = []
tsv cs =
  let (x,cs1) = break (=='\t') cs
  in x : if null cs1 then [] else tsv (tail cs1)

{-
main = do
  gr <- readPGF "../Parse.pgf"
  let Just eng = Map.lookup "ParseEng" (languages gr)
  dts <- fmap (readDepTrees eng . lines) $ readFile "ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-train.conllu"
  mapM_ print (depTreeUnigrams (dts !! 1))
-}
