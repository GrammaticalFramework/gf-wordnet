import PGF2
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

main = do
  es <- fmap (concatMap getExpr . lines) $ readFile "examples.txt"
  cfg <- readDepConfig "Parse.labels"
  let (unigrams,bigrams) = getStatistics cfg es
  writeFile "Parse.probs" 
            (unlines ([f++"\t"++show c | (f,c) <- Map.toList unigrams] ++
                      [h++"\t"++lbl++"\t"++m++"\t"++show c | ((h,lbl,m),c) <- Map.toList bigrams]))

getExpr l 
  | take 4 l == "abs:" = case readExpr (drop 5 l) of
                           Just e  -> [e]
                           Nothing -> []
  | otherwise          = []

type Label = String
type DepConfig = Map.Map Fun [(Label,[Pragma])]
data Pragma
       = Anchor
       | DepIndex Int
       | Reverse
       | Relation Label
       deriving Show

readDepConfig :: FilePath -> IO DepConfig
readDepConfig fpath =
  fmap (Map.fromList . concatMap toEntry . lines) $ readFile fpath
  where
    toEntry l =
      case words l of
        []       -> []
        ("--":_) -> []
        (fun:ws) -> [(fun,[toArgument w | w <- ws])]

    toArgument w =
      case words (map (\c -> if c == ':' then ' ' else c) w) of
        (w:ws) -> (w,map toPragma ws)

    toPragma "anchor"            = Anchor
    toPragma "rev"               = Reverse
    toPragma ('r':'e':'l':'=':w) = Relation w
    toPragma w                   = case reads w of
                                     [(n,"")] -> DepIndex n
                                     _        -> error ("Unknown pragma "++w)

type Statistics = (Map.Map Fun Int, Map.Map (Fun,Label,Fun) Int)

getStatistics :: DepConfig -> [Expr] -> Statistics
getStatistics config es = foldl' (\stats e -> fst $ getStats stats e) (Map.empty,Map.empty) es
  where
    getStats stats@(unigrams,bigrams) e =
      case unApp e of
        Just (f,[]) -> ((addUni f unigrams,bigrams), [f])
        Just (f,es) -> case Map.lookup f config of
                         Just labels -> let ((unigrams1,bigrams1),heads) = mapAccumL getStats stats es
                                            head                         = concat [mod | ((lbl,pragmas),mod) <- zip labels heads, lbl == "head"]
                                            unigrams2                    = addUni f unigrams1
                                            bigrams2                     = foldl' addDep bigrams1 [cross head lbl mod | ((lbl,pragmas),mod) <- zip labels heads, lbl /= "head"]
                                        in ((unigrams2,bigrams2),head)
                         Nothing     -> error ("Missing configuration for "++f)
        Nothing     -> (stats, [])

    cross head lbl mod = [(h,lbl,m) | h <- head, m <- mod]

    addDep = foldl' (flip (Map.alter (maybe (Just 1) (Just . (+1)))))
    addUni = Map.alter (maybe (Just 1) (Just . (+1)))
