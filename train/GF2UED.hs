module GF2UED(readDepConfig, expr2DepForest) where

import PGF2
import Data.List
import Data.Tree
import qualified Data.Map as Map

type Label = String
type DepConfig = Map.Map Fun [Pragma]
data Pragma
       = Head Int Label
       | Mod  Int Label
       | Rel  Int
       | Skip
       | Anch

readDepConfig :: FilePath -> IO DepConfig
readDepConfig fpath =
  fmap (Map.fromList . concatMap toEntry . lines) $ readFile fpath
  where
    toEntry l =
      case words l of
        []       -> []
        ("--":_) -> []
        (fun:ws) -> [(fun,[toPragma w | w <- ws])]

    toPragma "head"                   = Head 0         ""
    toPragma ('h':'e':'a':'d':':':cs) =
      case break (==':') cs of
        (lbl,[]    ) ->                 Head 0         lbl
        (lbl,':':cs) ->                 Head (read cs) lbl
    toPragma "rel"                    = Rel  0
    toPragma ('r':'e':'l':':':cs)     = Rel  (read cs)
    toPragma "_"                      = Skip
    toPragma "anchor"                 = Anch
    toPragma s                        =
      case break (==':') s of
        (lbl,[]    ) ->                 Mod  0         lbl
        (lbl,':':cs) ->                 Mod  (read cs) lbl

expr2DepForest :: DepConfig -> Expr -> Forest (Fun,Label)
expr2DepForest config e =
  let (ts,f) = e2f [] ["ROOT"] e in (map fst f++ts)
  where
    e2f ancs rootLbls e =
      case unApp e of
        Just (f,[]) -> (ancs,[(Node (f,lbl) [],"") | lbl <- rootLbls])
        Just (f,es) -> case Map.lookup f config of
                         Just pragmas -> mkForest ancs rootLbls (zip3 [1..] pragmas es)
                         Nothing      -> error ("Missing configuration for "++f)
        Nothing     -> (ancs,[])

    mkForest ancs rootLbls pes =
      (ancs',heads0)
      where
        (ancs',rels0,heads0,mods0) = process ancs pes

        process ancs []                    = (ancs,[],[],[])
        process ancs ((i,Rel j   ,e):pes)  =
          let (ancs1,f)                    = e2f ancs ["ROOT"] e
              (ancs2,rels,heads,mods)      = process ancs1 pes
              r                            = [(fst (rootLabel t),j) | (t,lbl) <- f]
          in (ancs2,r++rels,heads,mods)
        process ancs ((i,Head j lbl,e):pes)=
          let (ancs1,f)                    = e2f ancs (getLbls rootLbls 0) e
              (ancs2,rels,heads,mods)      = process ancs1 pes
              h                            = [(t,if null lbl then lbl' else lbl) | (t,lbl') <- attach f 0 i]
              m                            = [(Node (fun,lbl) mods,j) | (Node (fun,_) mods,_) <- h, j/=0]
          in (ancs2,rels,h++heads,m++mods)
        process ancs ((i,Mod  j lbl,e):pes)=
          let (ancs1,f)                    = e2f ancs (getLbls [lbl] i) e
              (ancs2,rels,heads,mods)      = process ancs1 pes
              m                            = [(t,j) | (t,"") <- attach f i i]
              a                            = [t | t <- attachH (attach f i i)]
          in (a++ancs2,rels,heads,m++mods)
        process ancs ((i,Anch    ,e):pes)  =
          let (ancs1,f)                    = e2f ancs (getLbls ["ROOT"] i) e
              (ancs2,rels,heads,mods)      = process (map fst (attach f i i)++ancs1) pes
          in (ancs2,rels,heads,mods)
        process ancs ((i,Skip    ,e):pes)  = process ancs pes

        attach ts k l = [(Node x (mods'++[t | (t,j) <- mods0, j==k || j==l]),lbl) | (Node x mods',lbl) <- ts]
        
        attachH ts = [Node (fun,"ROOT") (mods'++[Node (fun,lbl) mods | (Node (fun,_) mods,_) <- heads0]) | (Node (fun,_) mods',lbl) <- ts, lbl/=""]

        getLbls lbls idx =
          case [fun | (fun,idx') <- rels0, idx' == idx] of
            []   -> lbls
            lbls -> lbls


main = do
  config <- readDepConfig "../Parse.labels"
  ls <- fmap lines $ readFile "../examples.txt"
  let es = [e | l <- ls, take 4 l == "abs:", Just e <- [readExpr (drop 4 l)]]
  -- let Just e = readExpr "AdvRAP (AdjCN (ConjAP and_Conj (ConsAP (PositA smart_1_A) (BaseAP (PositA beautiful_1_A) (PositA modest_1_A)))) (UseN girl_N)) for_Prep (UsePN britain_PN)"
  -- let Just e = readExpr "AdjCN (ComparA smart_1_A (UsePN britain_PN)) (UseN girl_N)"
  -- let Just e = readExpr "AdjCN (PositA beatuful_1_A) (AdjCN (PastPartAgentAP (SlashV2a play_1_V2) (UsePN britain_PN)) (UseN song_1_N))"
  let Just e = readExpr "PhrUtt NoPConj (UttS (UseCl (TTAnt TPast ASimul) PPos (PredVP (DetCN (DetQuant DefArt NumSg) (UseN preface_N)) (ComplSlash (SlashV2a contain_1_V2) (DetCN (DetQuant IndefArt NumSg) (PossNP (UseN acknowledgement_2_N) (RelNP (UseDAPFem (DetDAP (DetQuant that_Quant NumPl))) (UseRCl (TTAnt TPast AAnter) PPos (RelVP IdRP (ComplSlash (SlashV2a help_2_V2) (UsePron she_Pron))))))))))) NoVoc"
  mapM_ (process config) es
  where
    process config e = do
      putStrLn (showExpr [] e)
      mapM_ (putStrLn . drawTree . fmap show) (expr2DepForest config e)
