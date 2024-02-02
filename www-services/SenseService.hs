{-# LANGUAGE CPP, BangPatterns, MonadComprehensions #-}
import PGF2
import Database.Daison
import SenseSchema
import ContentSchema
import Interval
import PatternMatching
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Control.Monad(foldM,msum,forM_,liftM2,liftM3,liftM4,forM)
import Control.Concurrent
import Control.Applicative((<|>))
import Network.HTTP
import Network.HTTP.Cookie
import Text.JSON
import Text.JSON.String
import Data.Maybe
import Data.List(sortOn,sortBy,intercalate,nub)
import Data.Char
import System.IO hiding (ReadWriteMode)
import System.Environment
import System.Directory
import System.Posix.Files
import System.Random
import System.Process
import System.Exit
import qualified Codec.Binary.UTF8.String as UTF8 (encodeString,decodeString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (toString,fromString)
import qualified Data.ByteString.UTF8 as BSS
import OpenSSL


main = do
  db <- openDB (DOC_PATH++"/semantics.db")
  bigram_total <- runDaison db ReadOnlyMode $ do
    query sumRows
          [c*c
             | (ex_id,(ex,_)) <- from examples everything
             , let c = length (exprFunctions ex)]
  withOpenSSL (simpleServer (Just 41297) Nothing (cgiMain db bigram_total))
  closeDB db


maxResultLength = 500


cgiMain :: Database -> Int -> Request -> IO Response
cgiMain db bigram_total rq = do
  let query = rqQuery rq
      mb_s1 = lookup "lexical_ids" query
      mb_s2 = lookup "context_id" query
      mb_s3 = lookup "gloss_id" query
      mb_s4 = lookup "depth" query
      mb_s7 = lookup "generalize_ids" query
      mb_s8 = lookup "list_domains" query
      s9    = [value | ("domain",value) <- query]
      mb_s10 = lookup "list_top_classes" query
      mb_s11 = lookup "class_id" query
      s12   = [value | ("pattern_match",value) <- query]
      mb_s21 = lookup "code" query
      mb_s22 = lookup "user" query
      mb_s23 = lookup "update_id" query
      mb_s24 = lookup "get_id" query
      mb_s25 = lookup "lang" query
      mb_s26 = lookup "def" query
      mb_s27 = lookup "commit" query
      mb_s28 = lookup "push" query
      mb_s29 = lookup "author" query
      mb_s210 = lookup "token" query
      mb_s211 = lookup "pick_example" query
      mb_s212 = lookup "update_example" query

  case mb_s1 of
    Just s  -> do json <- doQuery (words s)
                  outputJSONP query json
    Nothing -> case mb_s2 of
                 Just lex_id -> do json <- doContext lex_id (fromMaybe 4 (fmap read mb_s4))
                                   outputJSONP query json
                 Nothing     -> case mb_s3 of
                                  Just lex_id -> do json <- doGloss lex_id
                                                    outputJSONP query json
                                  Nothing     -> case mb_s7 of
                                                   Just s  -> do json <- doGeneralize (words s)
                                                                 outputJSONP query json
                                                   Nothing -> case mb_s8 of
                                                                Just _  -> do json <- doListDomains
                                                                              outputJSONP query json
                                                                Nothing -> case map read s9 of
                                                                             (d:ds) -> do json <- doDomainQuery d ds
                                                                                          outputJSONP query json
                                                                             _      -> case mb_s10 of
                                                                                         Just _  -> do json <- doListTopClasses
                                                                                                       outputJSONP query json
                                                                                         Nothing -> case mb_s11 of
                                                                                                      Just id -> do json <- doClassQuery (read id)
                                                                                                                    outputJSONP query json
                                                                                                      Nothing -> case s12 of
                                                                                                                   _:_ -> do case decode (rqBody rq) of
                                                                                                                               Ok pattern -> do json <- doPatternMatch s12 pattern
                                                                                                                                                outputJSONP query json
                                                                                                                               Error msg   -> do fail msg
                                                                                                                   []  -> case mb_s21 of
                                                                                                                            Just code -> doLogin code
                                                                                                                            Nothing   -> case liftM3 doGet mb_s22 mb_s24 mb_s25 of
                                                                                                                                               Just action -> do res <- action
                                                                                                                                                                 case res of
                                                                                                                                                                   Just def -> outputJSONP query (showJSON def)
                                                                                                                                                                   Nothing  -> httpError 404 "Not Found" ""
                                                                                                                                               Nothing     -> case liftM3 doUpdate mb_s22 mb_s23 mb_s25 of
                                                                                                                                                                Just action -> do json <- action mb_s26
                                                                                                                                                                                  outputJSONP query json
                                                                                                                                                                Nothing     -> case liftM4 doCommit mb_s22 mb_s29 mb_s210 mb_s27 of
                                                                                                                                                                                 Just action -> do res <- action
                                                                                                                                                                                                   return (Response
                                                                                                                                                                                                             { rspCode = 200
                                                                                                                                                                                                             , rspReason = "OK"
                                                                                                                                                                                                             , rspHeaders = [Header HdrCacheControl "no-cache"
                                                                                                                                                                                                                            ,Header (HdrCustom "X-Content-Type-Options") "nosniff"
                                                                                                                                                                                                                            ,Header HdrContentType "text/plain; charset=UTF8"
                                                                                                                                                                                                                            ]
                                                                                                                                                                                                             , rspBody = res
                                                                                                                                                                                                             })
                                                                                                                                                                                 Nothing     -> case fmap doPull mb_s28 of
                                                                                                                                                                                                  Just action -> do s <- action
                                                                                                                                                                                                                    outputText s
                                                                                                                                                                                                  Nothing     -> case fmap doPickExample mb_s211 of
                                                                                                                                                                                                                    Just action -> action >>= outputJSONP query
                                                                                                                                                                                                                    Nothing     -> case liftM3 doUpdateExample mb_s22 (fmap read mb_s212) mb_s26 of
                                                                                                                                                                                                                                     Just action -> action >>= outputJSONP query
                                                                                                                                                                                                                                     Nothing     -> httpError 404 "Not Found" "Unknown command"
  where
    doQuery lex_ids = do
      senses <- runDaison db ReadOnlyMode $
                  foldM (getSense db) Map.empty lex_ids
      let sorted_senses = (sortSenses . Map.toList) senses
      return (makeObj [("total",     showJSON (length lex_ids))
                      ,("retrieved", showJSON (length lex_ids))
                      ,("result",    showJSON (map mkSenseObj sorted_senses))
                      ])
      where
        getSense db senses lex_id = do
          lexemes <- select (fromIndex lexemes_fun (at lex_id))
          foldM getGloss senses lexemes

    doContext lex_id depth = do
      runDaison db ReadOnlyMode $ do
        ctxt <- query (groupRowsWith (+) 0)
                      [(lex_id', 1/(lex_prob lex * lex_prob lex' * fromIntegral bigram_total))
                          | (_,lex) <- fromIndex lexemes_fun (at lex_id)
                          , (ex_id,(ex,_)) <- fromIndex examples_fun (at lex_id)
                          , lex_id' <- anyOf (exprFunctions ex)
                          , lex_id' /= lex_id
                          , (_,lex') <- fromIndex lexemes_fun (at lex_id')]
                  `having` (\x -> x > 1)
        synsets <- select [synset_id
                             | (_,lex) <- fromIndex lexemes_fun (at lex_id)
                             , Just synset_id <- return (synset lex)]
        graph <- foldM (crawlGraph 0 depth) Map.empty synsets
        return (makeObj [("context", showJSON (map mkFunProb (Map.toList ctxt)))
                        ,("synsets", showJSON synsets)
                        ,("graph",   makeObj [(show key,mkNode node) | (key,node) <- Map.toList graph])
                        ])
      where
        dist v1 v2 = sqrt (Vector.sum (Vector.zipWith diff v1 v2))
          where
            diff x y = (x-y)^2

        mkFunProb (fun,prob) = makeObj [("mod", showJSON fun),("prob", showJSON (log prob))]

        mkNode (gloss,funs,ptrs,dist) =
          makeObj [("gloss",showJSON gloss)
                  ,("funs", showJSON funs)
                  ,("ptrs", showJSON [(show sym,showJSON id) | (sym,id) <- ptrs])
                  ,("dist", showJSON dist)
                  ]

    doGloss lex_id = do
      glosses <- runDaison db ReadOnlyMode $
                    select [gloss s | (_,lex@(Lexeme{synset=Just synset_id})) <- fromIndex lexemes_fun (at lex_id),
                                      s <- from synsets (at synset_id)]
      return (showJSON glosses)

    doGeneralize ids = do
      x <- runDaison db ReadOnlyMode $ fmap catMaybes $ do
         select [synset lexeme | fun <- anyOf ids,
                                 (_,lexeme) <- fromIndex lexemes_fun (at fun)]

      let up synset_id =
            runDaison db ReadOnlyMode $ fmap head $ do
              select [[id | (rel,id) <- pointers s
                          , elem rel [Hypernym, InstanceHypernym]]
                              | s <- from synsets (at synset_id)]

      ids <- findLCA up (nub x)

      fs <- runDaison db ReadOnlyMode $ do
               select [(synset_id,(gloss,lex_ids))
                          | int <- query (foldRows intersection [(0,maxBound-1)])
                                         [children s | synset_id <- anyOf ids,
                                                       s <- from synsets (at synset_id)],
                            size int < 2000,
                            (s,e) <- anyOf int,
                            (synset_id,Synset offset _ _ gloss) <- from synsets (asc ^>= s ^<= e),
                            lex_ids <- select [(lex_id,status,frame_inf,Just (domains,images,examples,sexamples,ptrs))
                                                   | (_,Lexeme lex_id _ status _ domain_ids ex_ids fs ptrs0 images) <- fromIndex lexemes_synset (at synset_id),
                                                     domains   <- select [makeObj [ ("id",showJSON domain_id)
                                                                                  , ("name",showJSON (domain_name d))
                                                                                  ]
                                                                            | domain_id <- anyOf domain_ids
                                                                            , d <- from domains (at domain_id)],
                                                     examples  <- select [ex | ex_id <- anyOf ex_ids, ex <- from examples (at ex_id)],
                                                     sexamples <- select [ex | (id,ex) <- fromIndex examples_fun (at lex_id), not (elem id ex_ids)],
                                                     ptrs <- select [(sym,lex_fun lex,SenseSchema.status lex) | (sym,id) <- anyOf ptrs0, lex <- from lexemes (at id)],
                                                     frame_inf <- select [(name cls,base_class_id f,(frame_id,pattern f,semantics f,Nothing))
                                                                            | frame_id <- anyOf fs
                                                                            , f <- from frames (at frame_id)
                                                                            , cls <- from classes (at (base_class_id f))]
                                                   ]]

      return  (makeObj [("concepts",  showJSON ids)
                       ,("result",    showJSON (map mkSenseObj fs))
                       ])


    doListDomains =
      runDaison db ReadOnlyMode $ do
        roots <- listDomains 0
        return (showJSON roots)
      where
        listDomains :: QueryMonad m => Key Domain -> m [JSValue] 
        listDomains parent =
          select [makeObj [("id",   showJSON id)
                          ,("name", showJSON (domain_name domain))
                          ,("children", showJSON children)
                          ]
                     | (id,domain) <- fromIndex domains_parent (at parent)
                     , children <- listDomains id
                     ]

    doDomainQuery d ds = do
      runDaison db ReadOnlyMode $ do
        lexemes0 <- select [res | res@(_,lexeme) <- fromIndex lexemes_domain (at d),
                                  all (flip elem (domain_ids lexeme)) ds]
        let lexemes1 = take maxResultLength lexemes0
        senses <- foldM getGloss Map.empty lexemes1
        let sorted_senses = (sortSenses . Map.toList) senses
        return (makeObj [("total",     showJSON (length lexemes0))
                        ,("retrieved", showJSON (length lexemes1))
                        ,("result",    showJSON (map mkSenseObj sorted_senses))
                        ])

    doListTopClasses = do
      x <- runDaison db ReadOnlyMode $ do
         select [(id,name cls) | (id,cls) <- from classes everything, isNothing (super_id cls)]
      return (showJSON x)
      
    doClassQuery id = do
      x <- runDaison db ReadOnlyMode $ do
         select [mkClassObj cls frames subclasses
                   | cls <- from classes (at id),
                     frames <- select (getFrames id),
                     subclasses <- select (getChildren id)]
      return (showJSON x)
      where
        getChildren super_id =
          [mkClassObj cls frames subclasses 
             | (id,cls)  <- fromIndex classes_super (at super_id),
               frames <- select (getFrames id),
               subclasses <- select (getChildren id)
             ]

        getFrames class_id =
          [(id,pattern frm,semantics frm,Just (map (lex_fun . snd) lexemes))
             | (id,frm) <- fromIndex frames_class (at class_id),
               lexemes <- select (fromIndex lexemes_frame (at id))]

    doPatternMatch vars pattern = do
      res <- runDaison db ReadOnlyMode $ do
               select [makeObj [(var,mkSenseObj (binding2obj value))
                                    | var <- vars
                                    , Just value <- [Map.lookup var env]]
                          | env <- matchPattern pattern]
      return (showJSON res)
      where
        binding2obj (LexemeValue _ lexeme mb_synset) =
          (fromMaybe 0 (synset lexeme),(maybe "" gloss mb_synset,[(lex_fun lexeme,status lexeme,[],Nothing)]))
        binding2obj (SynsetValue key synset lexemes) =
          (key,(gloss synset,[(lex_fun lexeme,status lexeme,[],Nothing) | (_,lexeme) <- lexemes]))

    getGloss senses (_,Lexeme lex_id _ status mb_sense_id domain_ids ex_ids _ ptrs0 images) = do
      domains   <- select [makeObj [ ("id",showJSON domain_id)
                                   , ("name",showJSON (domain_name d))
                                   ]
                               | domain_id <- anyOf domain_ids
                               , d <- from domains (at domain_id)]
      examples  <- select [ex | ex_id <- anyOf ex_ids, ex <- from examples (at ex_id)]
      sexamples <- select [ex | (id,ex) <- fromIndex examples_fun (at lex_id), not (elem id ex_ids)]

      ptrs <- select [(sym,lex_fun lex,SenseSchema.status lex) | (sym,id) <- anyOf ptrs0, lex <- from lexemes (at id)]

      case mb_sense_id of
        Just sense_id ->
          case Map.lookup sense_id senses of
            Just (gloss,lex_ids)
                     -> return (Map.insert sense_id (gloss,addInfo lex_id (domains,images,examples,sexamples,ptrs) lex_ids) senses)
            Nothing  -> do [Synset _ _ _ gloss] <- select (from synsets (at sense_id))
                           lex_ids <- select [(lex_id,status,frame_inf,Nothing)
                                                  | (_,Lexeme lex_id _ status _ _ _ fs _ images) <- fromIndex lexemes_synset (at sense_id),
                                                    frame_inf <- select [(name cls,base_class_id f,(frame_id,pattern f,semantics f,Nothing))
                                                                              | frame_id <- anyOf fs
                                                                              , f <- from frames (at frame_id)
                                                                              , cls <- from classes (at (base_class_id f))]]
                           return (Map.insert sense_id (gloss,addInfo lex_id (domains,images,examples,sexamples,ptrs) lex_ids) senses)
        Nothing -> return (Map.insert (fromIntegral (5000000+Map.size senses)) ("",[(lex_id,status,[],Just (domains,[],examples,sexamples,ptrs))]) senses)
      where
        addInfo lex_id info lex_ids = 
          [(lex_id',status,frames,if lex_id == lex_id' then Just info else mb_info)
              | (lex_id',status,frames,mb_info) <- lex_ids]

    sortSenses = map snd . sortOn fst . map addKey
      where
        addKey (sense_id,(gloss,lex_ids)) = (fst (head key_lex_ids), (sense_id,(gloss,map snd key_lex_ids)))
          where
            key_lex_ids = sortOn fst [(toKey lex_id info,x) | x@(lex_id,_,_,info) <- lex_ids]

        toKey lex_id info = (isNothing info,reverse rid,reverse rcat,read ('0':reverse rn)::Int)
          where
            s0 = reverse lex_id
            (rcat,'_':s1) = break (=='_') s0
            (rn,rid) =
              case break (=='_') s1 of
                (s2,'_':s3) -> (takeWhile isDigit (dropWhile isAlpha s2),s3)
                _           -> ("0",s1)

    mkSenseObj (sense_id,(gloss,lex_ids)) =
      makeObj [("sense_id",showJSON sense_id)
              ,("gloss",showJSON gloss)
              ,("lex_ids",mkLexObj lex_ids)
              ]

    mkLexObj lex_ids =
      makeObj [(lex_id,mkInfObj status frames info) | (lex_id,status,frames,info) <- lex_ids]

    mkInfObj status frames info =
      makeObj (("status", mkStatusObj status) :
               ("frames", showJSON [(cid::String,bcid::Key Frame,mkFrameObj frame) | (cid,bcid,frame) <- frames]) :
               case info of
                 Nothing -> []
                 Just (domains,images,examples,sexamples,ptrs) -> [
                         ("match", showJSON True),
                         ("domains",  showJSON (domains :: [JSValue])),
                         ("images",  showJSON ([(url,img) | (_,url,img) <- images, not (null img)] :: [(String,String)])),
                         ("examples", showJSON (map mkExObj examples)),
                         ("secondary_examples", showJSON (map mkExObj sexamples)),
                         ("antonyms", makeObj [(id,makeObj [("status", mkStatusObj status)]) | (Antonym,id,status) <- ptrs]),
                         ("derived", makeObj [(id,makeObj [("status", mkStatusObj status)]) | (Derived,id,status) <- ptrs])
                         ])



    mkExObj (e,finsts) =
      makeObj [("expr", showJSON (showExpr [] e))
              ,("frames", showJSON (map (\(frame_id,roles) -> (frame_id::Key Frame,mkRolesObj roles)) finsts))
              ]

    mkRolesObj roles = makeObj (map (\(role,fid) -> (role,showJSON (fid :: FId))) roles)

    mkStatusObj status =
      makeObj [(lang,showJSON (map toLower (show (s :: Status)))) | (lang,s) <- status]

    mkClassObj cls frames subclasses =
      makeObj [("name",       showJSON (name cls))
              ,("vars",       showJSON (vars cls))
              ,("frames",     showJSON (map mkFrameObj frames))
              ,("subclasses", showJSON subclasses)
              ]

    mkFrameObj (id,pattern,semantics,mb_funs) =
      makeObj (("id",        showJSON (id :: Key Frame)) :
               ("pattern",   showJSON (showExpr [] pattern)) :
               ("semantics", showJSON (semantics :: String)) :
               case mb_funs of
                 Nothing -> []
                 Just funs -> [("fun", showJSON (funs :: [String]))]
              )

    doLogin code = do
      client_secret <- getEnv "GF_WORDNET_CLIENT_SECRET"
      let rq = insertHeader HdrAccept "application/json" $
               getRequest ("https://github.com/login/oauth/access_token?client_id=1e94c97e812a9f502068&client_secret="++client_secret++"&code="++code)
      rsp <- simpleHTTP rq
      case (do res <- runGetJSON readJSObject (rspBody rsp)
               obj <- case res of
                        JSObject obj -> Right obj
                        _            -> Left "Didn't get an object from api.github.com"
               resultToEither (valFromObj "access_token" obj)) of
        Right token -> do let rq = insertHeader HdrUserAgent "GF WordNet" $
                                   insertHeader HdrAuthorization ("token "++token) $
                                   getRequest "https://api.github.com/user"
                          rsp <- simpleHTTP rq
                          case (do res <- runGetJSON readJSObject (rspBody rsp)
                                   obj <- case res of
                                            JSObject obj -> Right obj
                                            _            -> Left "Didn't get an object from api.github.com"
                                   user  <- resultToEither (valFromObj "login" obj)
                                   name  <- resultToEither (valFromObj "name" obj <|> return user)
                                   email <- resultToEither (valFromObj "email" obj <|> return (user++"@github.com"))
                                   return (user, name++" <"++email++">")) of
                            Right (user,author) -> do count <- runDaison db ReadOnlyMode $
                                                                  fmap length $ select (from updates_usr everything)
                                                      let path = "/wordnet"
                                                      return (Response
                                                                { rspCode = 302
                                                                , rspReason = "Found"
                                                                , rspHeaders = [Header HdrLocation path]
                                                                , rspBody = ""
                                                                }
                                                              `setCookies`
                                                              [(mkSimpleCookie "user" user){ckPath=Just path}
                                                              ,(mkSimpleCookie "author" author){ckPath=Just path}
                                                              ,(mkSimpleCookie "token" token){ckPath=Just path}
                                                              ,(mkSimpleCookie "count" (show count)){ckPath=Just path}
                                                              ])
                            Left msg     -> httpError 400 "https://api.github.com/user" msg
        Left msg -> httpError 400 "Invalid response from https://github.com/login/oauth/access_token" msg

    doGet user lex_id lang = do
      res <- runDaison db ReadWriteMode $ do
               select (fromIndex updates_lex_idx (at (user,lex_id,lang)))
      case res of
        (_,u):_ -> return (Just (def u))
        _       -> fmap (Map.lookup lex_id) (getDefinitions (Set.singleton lex_id) lang)

    doUpdate user lex_id lang mb_def = do
      def <- case mb_def of
               Just def -> return def
               Nothing  -> do mb_def <- doGet user lex_id lang
                              return (fromMaybe "" mb_def)
      runDaison db ReadWriteMode $ do
        res <- update lexemes [(id, lex{status=updateStatus lang Checked (status lex)}) | (id,lex) <- fromIndex lexemes_fun (at lex_id)]
        insert_ updates (UpdateLexeme user lex_id lang def)
        c <- query countRows (from updates_usr everything)
        return (c,head [map toLower (show st)
                              | (_,lexeme) <- res,
                                (lang',st) <- status lexeme,
                                lang==lang'])
      where
        updateStatus lang s []                  = [(lang,s)]
        updateStatus lang s ((lang',s'):status)
          | lang==lang'                         = (lang,s) : status
        updateStatus lang s (x:status)          = x : updateStatus lang s status

    doCommit user author token commit = do
      (out,inp) <- createPipe
      forkIO (patchUp inp >>
              hPutStrLn inp "" >>
              hFlush inp >>
              git inp [["commit","--author",author,"--message","progress","WordNet*.gf","examples.txt"]
                      ,["push", "https://"++user++":"++token++"@github.com/GrammaticalFramework/gf-wordnet"]
                      ])
      hGetContents out
      where
        patchUp inp = do
          res <- runDaison db ReadWriteMode $ do
                   res <- query groupRows [(fileName u,u) | (_,u) <- fromIndex updates_usr (at user)]
                   delete updates (from updates_usr (at user))
                   return res
          forM_ (Map.toList res) $ \(fname,us) ->
            if null us
              then return ()
              else do hPutStrLn inp ("Patch up "++fname)
                      hFlush inp
                      ls <- fmap lines $ readUtf8File fname
                      (tmp_fname,hTmp) <- openTempFile SERVER_PATH fname
                      hSetEncoding hTmp utf8
                      mapM_ (hPutStrLn hTmp) (annotate fname us 1 ls)
                      hClose hTmp
                      setFileMode tmp_fname (ownerReadMode `unionFileModes`
                                             ownerWriteMode `unionFileModes`
                                             groupReadMode `unionFileModes`
                                             groupWriteMode `unionFileModes`
                                             otherReadMode)
                      renameFile tmp_fname (SERVER_PATH++"/"++fname)

        fileName (UpdateLexeme _ lex_id lang def) = "WordNet"++drop 5 lang++".gf"
        fileName (UpdateExample _ _ _)            = "examples.txt"

        annotate fname updates !line_no []     = []
        annotate fname updates !line_no (l:ls)
          | fname == "examples.txt"            =
              case [def | UpdateExample _ no def <- updates, line_no==no] of
                (def:_) -> let def_ls = lines def
                               n      = length def_ls
                           in def_ls ++ annotate fname updates (line_no+n) (drop n (l:ls))
                _       -> l : annotate fname updates (line_no+1) ls
        annotate fname updates !line_no (l:ls) =
          (case words l of
             ("lin":id:"=":_) -> case [def | UpdateLexeme _ lex_id lang def <- updates, lex_id==id] of
                                   (def:_) -> "lin "++id++" = "++def++" ;"
                                   _       -> l
             _                -> l)  : annotate fname updates (line_no+1) ls

    doPull _ = do
      (out,inp) <- createPipe
      git inp [["pull","--no-edit"]]
      hGetContents out

    doPickExample _ = do
      g  <- newStdGen
      ls <- fmap lines $ readUtf8File "examples.txt"
      return (showJSON (pick g 36000 1 ls))
      where
        pick :: StdGen -> Int -> Int -> [String] -> (Int,String)
        pick g n line_no ls = find i line_no ls
          where
            (i,g') = randomR (0,n) g

            find i !line_no []     = pick g' i 1 ls
            find i !line_no (l:ls)
              | take 8 l == "abs* Phr"
                                   = if i == 0
                                       then (line_no,unlines (l:take 4 ls))
                                       else find (i-1) (line_no+1) ls
              | otherwise          = find i (line_no+1) ls

    doUpdateExample user line_no def =
      case map (readExpr . drop 4) def_ls of
        (Just e : _) -> do let fns = exprFunctions e
                           lex_defs <- fmap Map.fromList (mapM (\lang -> fmap ((,) lang)
                                                                              (getDefinitions (Set.fromList fns) lang))
                                                               ["ParseBul","ParseSwe"])
                           runDaison db ReadWriteMode $ do
                             insert_ updates (UpdateExample user line_no def)
                             ex_id <- insert_ examples (e,[])
                             st <- fmap concat (mapM (updateLexeme ex_id lex_defs) fns)
                             c  <- query countRows (from updates_usr everything)
                             return (showJSON (c,st))
        _            -> fail "Invalid expression"
      where
        def_ls = lines def

        updateLexeme ex_id lex_defs lex_id = do
          res <- select (fromIndex lexemes_fun (at lex_id))
          forM res $ \(id,lex) -> do
            status' <- mapM check (status lex)
            store lexemes (Just id)
                          (lex{status     =status',
                               example_ids=if elem lex_id (words (last def_ls)) && not (elem ex_id (example_ids lex))
                                             then ex_id:example_ids lex
                                             else example_ids lex
                              })
            return (lex_id,[(lang,map toLower (show st)) | (lang,st) <- status', Map.member lang lex_defs])
          where
            check (lang,st)
              | st == Unchecked || st == Guessed
                          = case Map.lookup lang lex_defs >>= Map.lookup lex_id of
                              Just def -> do insert_ updates (UpdateLexeme user lex_id lang def)
                                             return (lang,Checked)
                              Nothing  -> do return (lang,st)
              | otherwise = return (lang,st)

findLCA :: (Monad m, Ord a) => (a -> m [a]) -> [a] -> m [a]
findLCA up xs = alternate [([x],[]) | x <- xs] [] [] Map.empty
  where
    n = length xs

    alternate []              []  zs set = return zs
    alternate []              ss2 zs set = alternate ss2 []  zs set
    alternate (([],  []):ss1) ss2 zs set = alternate ss1 ss2 zs set
    alternate (([],  ys):ss1) ss2 zs set = alternate ((reverse ys, []):ss1) ss2 zs set
    alternate ((x:xs,ys):ss1) ss2 zs set
      | Map.lookup x set == Just n       = alternate ss1 ((xs,ys):ss2) zs set
      | otherwise                        = do 
          ws <- up x
          (ys,zs,set) <- ascend ws ys zs set
          alternate ss1 ((xs,ys):ss2) zs set

    ascend []     ys zs set = return (ys,zs,set)
    ascend (x:xs) ys zs set
      | c == n           = ascend xs ys   (x:zs) set'
      | otherwise        = ascend xs (x:ys)  zs  set'
      where
        c    = fromMaybe 0 (Map.lookup x set) + 1
        set' = Map.insert x c set


type Graph   = Map.Map (Key Synset) (String,[Fun],[(PointerSymbol,Key Synset)],Int)

crawlGraph :: Int -> Int -> Graph -> Key Synset -> Daison Graph
crawlGraph dist depth graph synset_id
  | Map.member synset_id graph
                  = return (updateDepth graph)
  | dist >= depth = do details <- getDetails False synset_id
                       return (addDetails details graph)
  | otherwise     = do details@(gloss,funs,ptrs,_) <- getDetails True synset_id
                       foldM (crawlGraph (dist+1) depth) (addDetails details graph) (map snd ptrs)
  where
    getDetails use_new synset_id = do
      (gloss,ptrs) <- query firstRow
                          [( gloss synset
                           , [ptr | ptr@(sym,tgt) <- pointers synset
                                  , match synset_id tgt]
                           )
                              | synset <- from synsets (at synset_id)
                              ]
      funs <- select [lex_fun lex
                        | (_,lex) <- fromIndex lexemes_synset (at synset_id)]
      return (gloss,funs,ptrs,dist)
      where
        match src tgt =
          case Map.lookup tgt graph of
            Just (_,_,ptrs,_) -> not (elem src (map snd ptrs))
            Nothing           -> use_new

    addDetails details graph = Map.insert synset_id details graph
    
    updateDepth graph = Map.adjust (\(gloss,funs,ptrs,_) -> (gloss,funs,ptrs,dist)) synset_id graph


outputJSONP q r = do
  let (ty,str) = case lookup "jsonp" q of
                   Nothing -> ("json",encode r)
                   Just c  -> ("javascript",c ++ "(" ++ encode r ++ ")")
  return (Response
            { rspCode = 200
            , rspReason = "OK"
            , rspHeaders = [Header HdrContentType ("application/"++ty++"; charset=utf-8")]
            , rspBody = str
            })


git inp []                 = hClose inp
git inp (command:commands) = do
  hPutStrLn inp (unwords ("$":"git":map censor command))
  hFlush inp
  (_,_,_,ph) <- createProcess_ "git"
                               (proc "git" command){std_out=UseHandle inp
                                                   ,std_err=UseHandle inp
                                                   ,cwd=Just SERVER_PATH
                                                   }
  code <- waitForProcess ph
  case code of
    ExitSuccess -> git inp commands
    _           -> hClose inp
  where
    censor ('h':'t':'t':'p':'s':':':'/':'/':cs) = 
      'h':'t':'t':'p':'s':':':'/':'/':drop 1 (dropWhile (/='@') cs)
    censor s = s

readUtf8File fname = do
  hInp <- openFile (SERVER_PATH++"/"++fname) ReadMode
  hSetEncoding hInp utf8
  hGetContents hInp

getDefinitions lex_ids lang = do
  s <- readUtf8File ("WordNet"++drop 5 lang++".gf")
  return (seek lex_ids s)
  where
    seek lex_ids _  | Set.null lex_ids     = Map.empty
    seek lex_ids []                        = Map.empty
    seek lex_ids ('\n':'l':'i':'n':' ':cs) = match lex_ids (dropWhile isSpace cs)
    seek lex_ids (c:cs)                    = seek lex_ids cs

    match lex_ids cs =
      let (id,cs1) = break isSpace cs
      in if Set.member id lex_ids
           then case dropWhile isSpace cs1 of
                  ('=':cs2) -> let cs3 = dropWhile isSpace cs2
                                   d   = def 0 cs3
                                   cs4 = drop (length d) cs3
                               in Map.insert id d (seek (Set.delete id lex_ids) cs4)
                  cs2       -> seek lex_ids cs2
           else seek lex_ids cs1

    def c []          = []
    def 0 (c@';' :cs) = []
    def i (c@'(' :cs) = c:def (i+1)         cs
    def i (c@'[' :cs) = c:def (i+1)         cs
    def i (c@'{' :cs) = c:def (i+1)         cs
    def i (c@')' :cs) = c:def (max 0 (i-1)) cs
    def i (c@']' :cs) = c:def (max 0 (i-1)) cs
    def i (c@'}' :cs) = c:def (max 0 (i-1)) cs
    def i (c@'"' :cs) = c:str i c cs
    def i (c@'\'':cs) = c:str i c cs
    def i (c@' ' :cs) = case def i cs of
                          [] -> []
                          cs -> c:cs
    def i (c     :cs) = c:def i cs

    str i d []          = []
    str i d ('\\':c:cs) = '\\':c:str i d cs
    str i d (     c:cs)
      | d == c             = c:def i   cs
      | otherwise          = c:str i d cs

