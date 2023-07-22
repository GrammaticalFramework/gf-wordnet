import PGF2
import Data.Char
import Data.Maybe
import Data.List(sortOn,intercalate)
import System.IO ( utf8 )
import Network.URI(escapeURIString,isUnreserved,unEscapeString)
import Network.HTTP
import Network.HTTP.MD5
import Debug.Trace
import qualified Data.Map as Map
import Text.EditDistance       -- pkg edit-distance

main = do
  gr <- readPGF "build/ParseEng.pgf"
  let Just eng = Map.lookup "ParseEng" (languages gr)
  wikidata <- runImageQuery query
  synsets <- fmap (Map.fromListWith (++) . mapMaybe parseAbsSyn . lines) $ readFile "WordNet.gf"
  res <- mapValuesWithKeyM (addImages eng wikidata) synsets
  mapM_ (mapM_ (putStrLn . showImages)) res
  where
    query = 
      "SELECT ?item ?itemLabel ?id ?sitelink ?image\n\
      \WHERE {\n\
      \  ?item wdt:P8814 ?id.\n\
      \  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n\
      \  OPTIONAL { ?sitelink schema:about ?item; schema:isPartOf <https://en.wikipedia.org/> }\n\
      \  OPTIONAL {\n\
      \    { ?item wdt:P18 ?image. BIND (1 as ?rank) }\n\
      \    UNION\n\
      \    { ?item wdt:P6802 ?image. BIND (2 as ?rank) }\n\
      \    UNION\n\
      \    { ?item wdt:P117 ?image. BIND (3 as ?rank) }\n\
      \    UNION\n\
      \    { ?item wdt:P8224 ?image. BIND (4 as ?rank) }\n\
      \    UNION\n\
      \    { ?item wdt:P242 ?image. BIND (5 as ?rank) }\n\
      \    UNION\n\
      \    { ?item wdt:P41 ?image. BIND (6 as ?rank) }\n\
      \    UNION\n\
      \    { ?item wdt:P94 ?image. BIND (7 as ?rank) }\n\
      \  }\n\
      \}\n\
      \ORDER BY ?rank"
      
mapValuesWithKeyM :: (Ord a,Monad m) => (a -> [b] -> m [c]) -> Map.Map a [b] -> m (Map.Map a [c])
mapValuesWithKeyM f m = fmap Map.fromAscList (mapMaybeM (Map.toAscList m))
  where
    mapMaybeM []          = return []
    mapMaybeM ((k,vs):xs) = do vs <- f k vs
                               case vs of
                                 [] -> mapMaybeM xs
                                 vs -> do xs <- mapMaybeM xs
                                          return ((k,vs):xs)

showImages (lemma,imgs) =
  lemma++"\t"++intercalate "\t" [qid++";"++escape url++";"++escape img | (qid,url,img) <- imgs]
  where
    escape []       = []
    escape (';':cs) = "%3B" ++ escape cs
    escape (c  :cs) = c      : escape cs

addImages eng wikidata synset lemmas = do
  mb_entities <- if take 1 synset == "Q"
                   then do wikidata <- runImageQuery (query synset)
                           let uri = "http://www.wikidata.org/entity/"++synset
                           return (Map.lookup uri wikidata)
                   else do return (Map.lookup synset wikidata)
  case mb_entities of
    Nothing       -> return []
    Just entities -> return (group [select
                                      [(d,(lemma,[(qid,url,img)])) | lemma <- lemmas
                                                                   , let d = distance lemma lbl]
                                      | (qid,lbl,url,img) <- entities])
  where
    select xs = 
      let (lemma,imgs) = (snd . head . sortOn fst) xs
      in case flip lemma of
           Nothing     -> [(lemma,imgs)]
           Just lemma' -> [(lemma,imgs),(lemma',imgs)]
      where
        flip []                          = Nothing
        flip ('M':'a':'s':'c':s@('_':_)) = Just ("Fem"++s)
        flip ('F':'e':'m':s@('_':_))     = Just ("Masc"++s)
        flip (x:xs)                      = fmap (x:) (flip xs)

    group = Map.toList . Map.fromListWith (++) . concat

    distance lemma lbl =
      levenshteinDistance defaultEditCosts (linearize eng (mkApp lemma [])) lbl
      
    query qid =
      "SELECT ?item ?itemLabel ?sense ?sitelink ?image WHERE\n\
      \{\n\
      \  BIND(wd:"++qid++" AS ?item)\n\
      \  BIND(wd:"++qid++" AS ?sense)\n\
      \  SERVICE wikibase:label { bd:serviceParam wikibase:language \"en\". }\n\
      \  OPTIONAL {\n\
      \    { wd:"++qid++" wdt:P18 ?image. BIND (1 as ?rank) }\n\
      \    UNION\n\
      \    { wd:"++qid++" wdt:P6802 ?image. BIND (2 as ?rank) }\n\
      \    UNION\n\
      \    { wd:"++qid++" wdt:P117 ?image. BIND (3 as ?rank) }\n\
      \    UNION\n\
      \    { wd:"++qid++" wdt:P8224 ?image. BIND (4 as ?rank) }\n\
      \    UNION\n\
      \    { wd:"++qid++" wdt:P242 ?image. BIND (5 as ?rank) }\n\
      \    UNION\n\
      \    { wd:"++qid++" wdt:P41 ?image. BIND (6 as ?rank) }\n\
      \    UNION\n\
      \    { wd:"++qid++" wdt:P94 ?image. BIND (7 as ?rank) }\n\
      \  }\n\
      \  OPTIONAL {\n\
      \    ?wikilink schema:about wd:"++qid++";\n\
      \    schema:isPartOf <https://en.wikipedia.org/>.\n\
      \  }\n\
      \  BIND(COALESCE(?wikilink, ?item) AS ?sitelink)\n\
      \}\n\
      \ORDER BY ?rank"


runImageQuery query = do
  let req  = insertHeader HdrAccept "text/tab-separated-values" $
             insertHeader HdrUserAgent "User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.102 Safari/537.36" $
             getRequest ("https://query.wikidata.org/sparql?query="++escapeURIString isUnreserved query)
  rsp <- simpleHTTP req
  return ((Map.fromListWith (++) . map parseEntry . drop 1 . lines) (rspBody rsp))
  where
    parseEntry l =
      case split '\t' l of
        (f1:f2:f3:f4:fs) ->
                         let qid   = init (tail f1)
                             label = (reverse . drop 4 . reverse) (tail f2)
                             sense = init (tail f3)
                             uri   = if null f4 then "" else init (tail f4)
                             img   = case fs of
                                       []   -> ""
                                       [""] -> ""
                                       [f5] -> let fname = init (drop 52 f5)
                                                   name  = map (\c -> if c == ' ' then '_' else c) (unEscapeString fname)
                                                   h     = md5ss utf8 name
                                               in "commons/"++take 1 h++"/"++take 2 h++"/"++name
                         in (sense,[(drop 31 qid,label,drop 30 uri,img)])

parseAbsSyn l =
  case words l of
    ("fun":fn:_) -> case break (=='\t') l of
                      (l1,'\t':l2) -> let synset = (reverse . dropWhile isSpace . take 10 . reverse) l1
                                      in Just (synset, [fn])
                      _            -> Nothing
    _            -> Nothing

split :: Char -> String -> [String]
split c "" = []
split c cs =
  let (x,cs1) = break (==c) cs
  in x : if null cs1 then [] else split c (tail cs1)
