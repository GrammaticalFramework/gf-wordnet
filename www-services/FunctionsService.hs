{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase, MonadComprehensions #-}
module FunctionsService(functionsService, pageService) where

import Control.Applicative     (liftA, liftA2, (<|>))
import Control.Monad (MonadPlus(mplus), foldM, forM, msum, (>=>))
import GF.Compile
import GF.Compile.Compute.Concrete2
import GF.Compile.TypeCheck.ConcreteNew
import qualified GF.Data.ErrM            as E
import GF.Grammar              hiding (VApp, VRecType, ppValue)
import GF.Grammar.Lookup
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Compile.Rename
import GF.Text.Pretty
import GF.Data.XML
import Network.HTTP
import Network.HTTP.MD5
import Network.URI
import OpenSSL
import PGF2
import System.IO ( utf8 )
import System.IO.Unsafe ( unsafePerformIO )
import System.FilePath
import System.Directory ( doesFileExist )
import Text.JSON
import Text.JSON.String (runGetJSON)
import Text.JSON.Types (get_field, JSObject(..), JSValue(..))
import Database.Daison
import SenseSchema

import qualified Data.ByteString.Char8   as BS
import qualified Data.Map                as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as BL
import Data.Char ( isDigit, ord, toLower )
import Data.Foldable ( find )
import Data.Functor ( (<&>) )
import Data.List ( singleton )
import Data.Maybe

functionsService :: Database -> PGF -> ModuleName -> SourceGrammar -> Request -> IO Response
functionsService db gr mn sgr rq =
  case (decode (rqBody rq) >>= parseQuery) <|> getFromQuery (rqQuery rq) of
    Ok f      -> f
    Error msg -> return (Response
                           { rspCode = 400
                           , rspReason = "Invalid input"
                           , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                           , rspBody = msg
                           })
  where
    parseQuery query = do
      mb_qid <- maybe (pure Nothing) (fmap Just . readJSON)
                      (lookup "qid" (fromJSObject query))
      lang <- valFromObj "lang" query
      code <- valFromObj "code" query
      cs <- case valFromObj "choices" query of
        Ok json -> orFailErr $ deserializeChoices json
        Error _ -> return Map.empty
      return (executeCode db gr sgr mn OutJSON mb_qid lang cs code)

    getFromQuery query = do
      lang <- orFail "No lang" $ lookup "lang" query
      code <- orFail "No code" $ lookup "code" query
      cs <- case lookup "choices" query of
        Just s  -> do json <- orFailE $ runGetJSON readJSArray s
                      orFailErr $ deserializeChoices json
        Nothing -> return Map.empty
      return $ executeCode db gr sgr mn OutJSON (lookup "qid" query) lang cs code

pageService :: Database -> PGF -> ModuleName -> SourceGrammar -> FilePath -> Request -> IO Response
pageService db gr mn sgr path rq = do
  (html_file,config) <- fmap read (readFile path)
  html <- readFile (dir </> html_file)
  let query = rqQuery rq
      lang  = fromMaybe "ParseEng" (lookup "lang" query)
  case lookup "qid" query of
    Just qid -> do rsp <- wikidataEntity qid
                   case rsp >>= get_classes of
                     Ok classes -> case [prog | cls <- classes, (cls',prog) <- config :: [(String,String)], cls==cls'] of
                                     (prog:_) -> do code <- readFile (dir </> prog)
                                                    rsp <- executeCode db gr sgr mn OutTable (Just qid) lang Map.empty code
                                                    let code_doc =
                                                          case lookup "edit" query of
                                                            Just _  -> showXMLDoc (Data code)
                                                            Nothing -> ""
                                                    if rspCode rsp == 200
                                                      then return rsp{rspBody=injectTemplate html prog code_doc (rspBody rsp)}
                                                      else return rsp
                                     []       -> return (Response
                                                           { rspCode = 200
                                                           , rspReason = "OK"
                                                           , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
                                                           , rspBody = injectTemplate html "" "" ("There is no renderer defined for classes "++unwords classes)
                                                           })
                     Error msg  -> return (Response
                                             { rspCode = 400
                                             , rspReason = "FAIL"
                                             , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
                                             , rspBody = msg
                                             })
    Nothing -> return (Response
                         { rspCode = 200
                         , rspReason = "OK"
                         , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
                         , rspBody = injectTemplate html "" "" ""
                         })
    where
      dir = dropFileName path

      get_classes json = do
        vals <- valFromObj "P31" json
        mapM (valFromObj "mainsnak" >=> valFromObj "datavalue" >=> valFromObj "value" >=> valFromObj "id") vals

      injectTemplate []                                           prog code output = []
      injectTemplate ('<':'%':'p':'r':'o':'g':'%':'>':cs)         prog code output = prog   ++ injectTemplate cs prog code output
      injectTemplate ('<':'%':'c':'o':'d':'e':'%':'>':cs)         prog code output = code   ++ injectTemplate cs prog code output
      injectTemplate ('<':'%':'o':'u':'t':'p':'u':'t':'%':'>':cs) prog code output = output ++ injectTemplate cs prog code output
      injectTemplate (c:cs)                                       prog code output = c :       injectTemplate cs prog code output

data ExecOutFormat = OutJSON | OutTable

executeCode :: Database      -- ^ Database for wiki data
            -> PGF           -- ^ Ambient core grammar
            -> SourceGrammar -- ^ Ambient grammar
            -> ModuleName    -- ^ Name of the predef module to open
            -> ExecOutFormat -- ^ Output format
            -> Maybe String  -- ^ Ambient QID
            -> String        -- ^ Ambient language
            -> ChoiceMap     -- ^ Initial choices (i.e. a program trace)
            -> String        -- ^ Code snippet to execute
            -> IO Response
executeCode db gr sgr mn fmt mb_qid lang csInit code =
  case runLangP NLG pNLG (BS.pack code) of
    Right prog ->
      case runCheck (checkComputeProg (maybe prog (add_qid prog) mb_qid)) of
        E.Ok (res,msg) -> case fmt of
          OutJSON  -> return (Response
                                { rspCode = 200
                                , rspReason = "OK"
                                , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                                , rspBody = encode $
                                              makeObj [("msg",showJSON msg)
                                                      ,("groups", showJSON [makeObj [("headers",showJSON headers),
                                                                                     ("dataset",JSArray [makeObj [ ("fields",showJSON fs)
                                                                                                                 , ("choices",serializeChoices cs)
                                                                                                                 , ("options",ois)
                                                                                                                 ]
                                                                                                           | (fs,cs,ois) <- dataset])]
                                                                              | (headers,dataset) <- res])
                                                      ]
                                })
          OutTable -> return (Response
                                { rspCode = 200
                                , rspReason = "OK"
                                , rspHeaders = [Header HdrContentType "text/html; charset=UTF8"]
                                , rspBody = concat [concat html | (headers,((html,_,_):_)) <- res] -- TODO options
                                })
        E.Bad msg  -> return (Response
                                { rspCode = 400
                                , rspReason = "Invalid Expression"
                                , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                                , rspBody = msg
                                })
    Left (Pn row col,msg)
                   -> return (Response
                                { rspCode = 400
                                , rspReason = "Parse Error"
                                , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                                , rspBody = (show row ++ ":" ++ show col ++ ":" ++ msg)
                                })
  where
    abs_mn = moduleNameS (abstractName gr)

    add_qid prog qid =
      Map.insert (identS "qid") (ResOper (Just (noLoc (Sort cStr))) (Just (noLoc (K qid)))) prog

    Just cnc = Map.lookup lang (languages gr)

    instantiate flag t (VProd Implicit x (VSort s) (VClosure env c t2))
      | s == cType = do
           i  <- newResiduation []
           g  <- globals
           let v2 = eval g ((x,VMeta i []):env) c t2 []
           instantiate True (App t (ImplArg (Meta i))) v2
    instantiate flag t ty = return (flag, t, ty)

    checkComputeProg jments = do
      let nlg_mn = moduleNameS (msrc nlg_mi)
          nlg_mi = ModInfo {
                     mtype   = MTResource,
                     mstatus = MSComplete,
                     mflags  = noOptions,
                     mextend = [],
                     mwith   = Nothing,
                     mopens  = [OSimple mn, OSimple abs_mn],
                     mexdeps = [],
                     msrc    = "Main",
                     mseqs   = Nothing,
                     jments  = jments
                   }
      let cwd = ""
      nlg_m <- renameModule cwd sgr (nlg_mn, nlg_mi)

      infoss <- checkInModule cwd nlg_mi NoLoc empty $ topoSortJments2 nlg_m
      let sgr'     = prependModule sgr nlg_m
          globals0 = Gl sgr' (wikiPredef db gr lang sgr')
      nlg_m <- foldM (foldM (checkInfo (mflags nlg_mi) cwd globals0)) nlg_m infoss
      checkWarn (ppModule Unqualified nlg_m)

      let sgr' = prependModule sgr nlg_m
          globals1 = Gl sgr' (wikiPredef db gr lang sgr')
          qident = (nlg_mn,identS "main")

      res <- runEvalMWithOpts globals1 csInit $ do
        g <- globals
        let (c1,c2) = split unit
        (term,res_ty) <- inferLType' (Q qident)
        (flag,term,res_ty) <- instantiate False term res_ty
        res <- value2termM True [] (eval g [] c2 term [])
        res <- case res of
                 FV ts -> msum (map return ts)
                 res   -> return res
        (res,res_ty) <- if flag
                          then inferLType' res
                          else return (res,res_ty)
        res_ty <- value2termM True [] res_ty
        res <- toRecord res_ty res
        return (toHeaders res_ty,res)
      res <- forM res $ \((hs,r),cs,ois) -> do
        ois <- orFailM "No result while serializing option info" $
          listToMaybe <$> runEvalM globals1 (serializeOptionInfo ois)
        return (hs,[(r,cs,ois)])
      return $ Map.toList (fmap reverse (Map.fromListWith (++) res))

    toHeaders (RecType lbls) = [toHeader (pp l <+> ':') ty | (l,ty) <- lbls]
    toHeaders ty             = [toHeader empty ty]

    toHeader d ty = JSONObject [("label",showJSON (render (d <+> ppTerm Unqualified 0 ty)))
                               ,("type", showJSON (toHeaderType ty))
                               ]

    toHeaderType (Sort s)
      | s == cStr                    = "string"
    toHeaderType (Q (m,c))
      | m == cPredef && c == cMarkup = "markup"
      | m == cPredef && c == cFloat  = "number"
      | m == cPredef && c == identS "Time" = "string"
    toHeaderType (App (Q (m,c)) _)
      | m == cPredef && c == cInts   = "number"
    toHeaderType ty                  = "text"

    toRecord (RecType lbls) (R as)  = toCells lbls as
    toRecord ty             t       = fmap singleton (toCell ty t)

    toCells []            as = return []
    toCells ((l,ty):lbls) as =
      case lookup l as of
        Just (_,t) -> do c  <- toCell ty t
                         cs <- toCells lbls as
                         return (c:cs)
        Nothing    -> do cs <- toCells lbls as
                         return ("?":cs)

    toCell (Sort s)  t
      | s == cStr =
          case toStr t of
            Just s  -> return s
            Nothing -> return (render (ppTerm Unqualified 0 t))
    toCell (Q (m,c)) t
      | m == cPredef && c == identS "Markup"
                       = do ts <- toXML t
                            return (foldr showsXML "" ts)
      | m == cPredef && c == identS "Time"
                       = case toStr t of
                           Just s  -> return s
                           Nothing -> return (render (ppTerm Unqualified 0 t))
    toCell (QC (m,c)) t
      | m == abs_mn = fmap (linearize cnc) (toExpr [] t)
    toCell ty        t
      | isPGFType ty = do e <- toExpr [] t
                          return (showExpr [] e)
      | otherwise    = return (render (ppTerm Unqualified 0 t))

    serializeOptionInfo ois = do
      rs <- forM ois $ \(OptionInfo c lty l os) -> do
        lty   <- value2termM True [] lty
        l     <- value2termM True [] l
        label <- toCell lty l
        os <- forM os $ \(oty,o) -> do
          oty <- value2termM True [] oty
          o   <- value2termM True [] o
          toCell oty o
        return $ makeObj [ ("label"  , showJSON label)
                         , ("choice" , showJSON (unchoice c))
                         , ("options", showJSON os)
                         ]
      return $ JSArray rs

    isPGFType (QC (m,c))
      | m == abs_mn = True
    isPGFType (Prod bt x t1 t2) = isPGFType t1 && isPGFType t2
    isPGFType _ = False

    toExpr xs (Abs bt x t) = liftA (EAbs bt (showIdent x)) (toExpr (x:xs) t)
    toExpr xs (Vr x)       = return (EVar (deBruijn 0 x xs))
                             where
                               deBruijn i x (x':xs)
                                 | x == x'   = i
                                 | otherwise = deBruijn (i+1) x xs
    toExpr xs (App t1 t2)  = liftA2 EApp (toExpr xs t1) (toExpr xs t2)
    toExpr xs (Q (_,c))    = return (EFun (showIdent c))
    toExpr xs (QC (_,c))   = return (EFun (showIdent c))
    toExpr xs (EInt n)     = return (ELit (LInt n))
    toExpr xs (EFloat d)   = return (ELit (LFlt d))
    toExpr xs (ImplArg t)  = liftA EImplArg (toExpr xs t)
    toExpr xs (Meta i)     = return (EMeta i)
    toExpr xs (FV ts)      = msum (map return ts) >>= toExpr xs
    toExpr xs t            = case toStr t of
                               Just s  -> return (ELit (LStr s))
                               Nothing -> return (EMeta 0)

    toStr (K s)        = Just s
    toStr (C t1 t2)    = do s1 <- toStr t1
                            s2 <- toStr t2
                            return (s1 ++ " " ++ s2)
    toStr (Glue t1 t2) = do s1 <- toStr t1
                            s2 <- toStr t2
                            return (s1 ++ s2)
    toStr _            = Nothing

    toXML (FV ts)      = msum (map return ts) >>= toXML
    toXML (Markup tag as ts)
      | tag == identW = fmap concat (mapM toXML ts)
      | otherwise     = do as <- mapM toAttr as
                           ts <- fmap concat (mapM toXML ts)
                           return [Tag (showIdent tag) as ts]
    toXML t           = case toStr t of
                          Just s  -> return [Data s]
                          Nothing -> do e <- toExpr [] t
                                        return [Data (linearize cnc e)]

    toAttr (id,FV ts) = do
      t <- msum (map return ts)
      toAttr (id,t)
    toAttr (id,t) =
      case toStr t of
        Just s  -> return (showIdent id, s)
        Nothing -> return (showIdent id, render (ppTerm Unqualified 0 t))

    checkInfo :: Options -> FilePath -> Globals -> SourceModule -> (Ident,Info) -> Check SourceModule
    checkInfo opts cwd globals sm (c,info) = checkInModule cwd (snd sm) NoLoc empty $ do
       case info of
         ResOper pty pde -> do
            info <- case (pty,pde) of
                (Just (L loct ty), Just (L locd de)) -> do
                     chIn locd "operation" $ do
                        fmap (mkInfo locd loct) $ runEvalM globals $ do
                           let (c1,c2,c3,c4) = split4 unit
                           (ty',_ ) <- checkLType' c1 ty (VSort cType)
                           let vty = eval globals [] c2 ty' []
                           (de,ty') <- checkLType' c3 de vty
                           ty <- value2termM False [] vty
                           return (de,ty)
                (Nothing         , Just (L locd de)) -> do
                     chIn locd "operation" $
                        fmap (mkInfo locd locd) (inferLType globals de)
                (Just (L loct ty), Nothing) -> do
                     chIn loct "operation" $
                        checkError (pp "No definition given to the operation")
            update sm c info
       where
         gr = prependModule sgr sm
         chIn loc cat = checkInModule cwd (snd sm) loc ("Happened in" <+> cat <+> c)

         update (mn,mi) c info = return (mn,mi{jments=Map.insert c info (jments mi)})
         
         mkInfo locd loct [(de',ty')] = (ResOper (Just (L locd ty')) (Just (L locd de')))
         mkInfo locd loct defs        = (ResOverload [] [(L locd ty',L locd de') | (de',ty') <- defs])

orFail s = maybe (fail s) pure

orFailM s = (orFail s =<<)
    
orFailE = either fail pure

orFailErr (E.Ok a)    = return a
orFailErr (E.Bad err) = fail err

serializeChoices :: ChoiceMap -> JSValue
serializeChoices cs = JSArray (Map.toList cs >>= \(c,i) -> [showJSON (unchoice c), showJSON i])

deserializeChoices :: JSValue -> E.Err ChoiceMap
deserializeChoices json = case readJSON json of
  Error err -> E.Bad err
  Ok cs     -> Map.fromList <$> parse cs
  where
    parse []       = E.Ok []
    parse [x]      = E.Bad "Choice array must have even length!"
    parse (c:i:cs) = do
      rs <- parse cs
      return $ (Choice c, fromInteger i) : rs

wikiPredef :: Database -> PGF -> String -> Grammar -> PredefTable
wikiPredef db pgf lang gr = Map.fromList
  [ (identS "entity", pdArity 2 $\ \g c [typ,qid] -> Const (fetch c typ qid))
  , (identS "int2digits", pdArity 1 $\ \g c [n] -> Const (int2digits abstr n))
  , (identS "int2decimal", pdArity 1 $\ \g c [n] -> Const (int2decimal abstr n))
  , (identS "float2decimal", pdArity 1 $\ \g c [f] -> Const (float2decimal abstr f))
  , (identS "int2numeral", pdArity 1 $\ \g c [n] -> Const (int2numeral abstr n))
  , (identS "expr", pdArity 2 $\ \g c [ty,qid] -> Const (get_expr lang c ty qid))
  , (identS "time2adv", pdArity 1 $\ \g c [time] -> Const (time2adv abstr time))
  , (identS "lang", pdArity 0 $\ \g c [] -> Const (VStr (map toLower (drop 5 lang))))
  , (cLessInt, pdArity 2 $\ \g c [v1,v2] -> fmap toBool (liftA2 (<) (value2int v1) (value2int v2)))
  ]
  where
    abstr = moduleNameS (abstractName pgf)

    fetch c typ (VStr qid) =
      case unsafePerformIO (wikidataEntity qid) of
        Ok obj    -> filterJsonFromType c obj typ
        Error msg -> VError (pp msg)
    fetch c ty (VFV c1 vs) = VFV c1 (mapVariantsC (\c -> fetch c ty) c vs)

    -- add lang -> synsets, give both options for lex and syn
    get_expr l c ty (VStr qid) =
      case res of
        [v] -> v
        vs  -> VFV c (VarFree vs)
      where
        res = unsafePerformIO $ 
                runDaison db ReadOnlyMode $ do
                  spec <- select [expr s | (i, s) <-  fromIndex qid2lang (at (qid, l))]
                  mul <- select [expr s | (i, s) <-  fromIndex qid2lang (at (qid, "ParseMul"))]
                  lexeme <- select [vapp abstr id [] | (_,lex) <- fromIndex lexemes_qid (at qid)
                                           , let id = lex_fun lex
                                           , fmap (matchType ty) (functionType pgf id) == Just True]
                  case spec of 
                        [] -> return $ lexeme ++ [eval globals0 [] c (toTerm [] (MN (i2i2 "Parse")) r) [] | r <- mul]
                        _  -> return $ lexeme ++ [eval globals0 [] c (toTerm [] (MN (i2i2 "Parse")) r) [] | r <- spec]
  
                                                    
        matchType (VProd bt1 _ ty11 ty2) (DTyp ((bt2,_,ty12):hypos) cat2 []) =
          bt1 == bt2 && matchType ty11 ty12 && matchType ty2 (DTyp hypos cat2 [])
        matchType (VApp _ (mod,cat1) []) (DTyp [] cat2 []) =
          mod == abstr && showIdent cat1 == cat2
        matchType (VMeta _ _) _ = True
        matchType _ _ = False
    get_expr l c ty (VFV c1 vs) = VFV c1 (mapVariantsC (\c -> get_expr l c ty) c vs)
    get_expr l c ty x           = VError (ppValue Unqualified 0 ty <+> ppValue Unqualified 0 x)

    globals0 = Gl gr Map.empty

    toTerm :: [Ident] -> ModuleName -> Expr -> Term
    toTerm scope l t = case t of 
      ELit (LInt i) -> EInt i
      ELit (LFlt f) -> EFloat f
      ELit (LStr s) -> K s
      EFun f -> Q (l, i2i2 f)
      EVar i -> Vr (scope !! i)
      EAbs b v e -> Abs b  (i2i2 v) (toTerm (identS v:scope) l e)
      EApp e1 e2 -> App (toTerm scope l e1) (toTerm scope l e2)
      EMeta i -> Meta i

i2i2 = identS

wikidataEntity qid = do
  rsp <- simpleHTTP (getRequest ("https://www.wikidata.org/wiki/Special:EntityData/"++qid++".json"))
  return (decode (rspBody rsp) >>= valFromObj "entities" >>= valFromObj qid >>= valFromObj "claims")

filterJsonFromType :: Choice -> JSObject [JSObject JSValue] -> Value -> Value
filterJsonFromType c obj typ =
  case typ of
   VRecType fields -> VR (mapC (\c -> getSpecificProperty c obj) c fields)
   VMeta _ _       -> VR (getAllProperties c obj)
   _               -> VError (pp "Wikidata entities are always records")

isProperty ('P':cs) = all isDigit cs
isProperty _        = False

getSpecificProperty :: Choice -> JSObject [JSObject JSValue] -> (Label, Value) -> (Label, Value)
getSpecificProperty c obj (LIdent field, typ)
  | isProperty label =
      case Text.JSON.Types.get_field obj label of
        Nothing    -> (LIdent field, VFV c (VarFree []))
        Just [obj] -> (LIdent field, transformJsonToValue typ c obj)
        Just objs  -> (LIdent field, VFV c (VarFree (mapC (transformJsonToValue typ) c objs)))
  | otherwise = (LIdent field, VError (pp field <+> "is an invalid Wikidata property"))
  where
    label = showRawIdent field

    transformJsonToValue :: Value -> Choice -> JSObject JSValue -> Value
    transformJsonToValue typ c obj =
      case fromJSObjectToValue c obj typ of
        Ok ass    -> VR ass
        Error msg -> VError (pp msg)
getSpecificProperty c obj (LVar n, typ) =
  (LVar n, VError (pp "Wikidata entities can only have named properties"))

getAllProperties :: Choice -> JSObject [JSObject JSValue] -> [(Label, Value)]
getAllProperties c obj =
  catMaybes $
     mapC (\c (label, objs) ->
                 let (c1,c2) = split c
                 in case mapCM parseVariant c1 objs of
                      Ok [v]    -> Just (LIdent (rawIdentS label), v)
                      Ok vs     -> Just (LIdent (rawIdentS label), VFV c2 (VarFree vs))
                      Error msg -> Nothing)
          c (fromJSObject obj)
  where
    parseVariant c obj = do
      (qs, dv, dt) <- parseWikiDataProp obj
      wdt <- getWikiDataType dt
      fs <- forM (wdtFields wdt) $ \f ->
        (,) (LIdent (rawIdentS (fieldName f))) <$> extractField f c Nothing dv
      return $ VR fs

fromJSObjectToValue :: Choice -> JSObject JSValue -> Value -> Result [(Label, Value)]
fromJSObjectToValue c obj typ = do
  (qs, dv, dt) <- parseWikiDataProp obj
  matchTypeFromJSON c qs dv dt typ

parseWikiDataProp :: JSObject JSValue -> Result ([(String, [JSObject JSValue])], JSObject JSValue, String)
parseWikiDataProp obj = do
  mainsnak <- valFromObj "mainsnak" obj
  datavalue <- valFromObj "datavalue" mainsnak
  datatype  <- valFromObj "datatype"  mainsnak
  qualifiers <- fmap fromJSObject (valFromObj "qualifiers" obj) `mplus` return []
  references <- fmap fromJSObject (valFromObj "references" obj) `mplus` return []
  return (qualifiers ++ references, datavalue, datatype)

data WikiDataFieldType = MkField
  { fieldName    :: String
  , fieldType    :: GF.Grammar.Type
  , extractField :: Choice -> Maybe Value -> JSObject JSValue -> Result Value
  }
newtype WikiDataType = WikiDataType { wdtFields :: [WikiDataFieldType] }

valField :: JSON a => String -> GF.Grammar.Type -> (Choice -> Maybe Value -> a -> Result Value) -> WikiDataFieldType
valField n ty f = MkField n ty $ \c ty -> valFromObj "value" >=> valFromObj n >=> f c ty

valField' :: JSON a => String -> GF.Grammar.Type -> (a -> Value) -> WikiDataFieldType
valField' n ty f = valField n ty (\c ty -> pure . f)

matchTypeFromJSON c qs dv dt (VRecType labels) = do
  wdt <- getWikiDataType dt
  mapCM (getField wdt) c labels
  where
    getField wdt c (k@(LIdent l),ty) = do
      let n = showRawIdent l
      val <-  case find (\f -> fieldName f == n) (wdtFields wdt) of
               Just f  -> extractField f c (Just ty) dv
               Nothing -> getQualifierOrReference c qs dt l ty
      return (k,val)
    getField _ _ _ = fail "Wikidata entities can only have named properties"
matchTypeFromJSON c qs dv dt (VMeta _ _) = do
  wdt <- getWikiDataType dt
  mapCM getField c (wdtFields wdt)
  where
    getField c f = do
      val <-  extractField f c Nothing dv
      return (LIdent (rawIdentS (fieldName f)),val)

getWikiDataType "commonsMedia"     = return commonsMediaWdt
getWikiDataType "quantity"         = return quantityWdt
getWikiDataType "wikibase-item"    = return wikibaseItemWdt
getWikiDataType "globe-coordinate" = return globeCoordinateWdt
getWikiDataType "time"             = return timeWdt
getWikiDataType "monolingualtext"  = return monolingualTextWdt
getWikiDataType dt                 = Error $ "Unknown WikiData type: " ++ dt

commonsMediaWdt = WikiDataType
  [ MkField "s" typeString $ \_ _ -> valFromObj "value" >=> \s -> return (VStr (constructImgUrl s))
  ]
  where
    constructImgUrl :: String -> String
    constructImgUrl img =
      let name = map (\c -> if c == ' ' then '_' else c) (unEscapeString img)
          h    = md5ss utf8 name    
      in "https://upload.wikimedia.org/wikipedia/commons/"++take 1 h++"/"++take 2 h++"/"++name

wikibaseItemWdt = WikiDataType
  [ valField' "id" typeString VStr
  ]

globeCoordinateWdt = WikiDataType
  [ valField' "latitude"  typeFloat VFlt
  , valField' "longitude" typeFloat VFlt
  , valField' "precision" typeFloat VFlt
  , valField' "altitude"  typeFloat VFlt
  , valField' "globe"     typeStr   VStr
  ]

quantityWdt = WikiDataType
  [ valField "amount" typeInt $ \c -> \case
      Just (VApp _ f [])
        | f == (cPredef,cInt)   -> valFromObj "value" >=> decimal VFlt
        | f == (cPredef,cFloat) -> valFromObj "value" >=> decimal VInt
        | otherwise             -> \_ -> fail "Not an Int or Float"
      _                         -> valFromObj "value" >=> decimal VInt
  , valField' "unit" typeString (VStr . dropURL)
  ]

cTime = identS "Time"

timeWdt = WikiDataType
  [ valField' "time"          (cnPredef cTime) VStr
  , valField' "precision"     typeInt          VInt
  , valField' "calendarmodel" typeString       (VStr . dropURL)
  ]

monolingualTextWdt = WikiDataType
  [ valField' "text"     typeString VStr
  , valField' "language" typeString VStr
  ]

getQualifierOrReference c qs dt l t
  | isProperty label =
        case lookup label qs of
          Just snaks -> let (c1,c2) = split c
                        in case [value | Ok value <- mapC get_value c2 snaks] of
                             [v] -> return v
                             vs  -> return (VFV c (VarFree vs))
          Nothing    -> return (VFV c (VarFree []))
  | otherwise = fail "An invalid Wikidata qualifier or reference"
  where
    label = showRawIdent l

    get_value c snak = do
      datavalue <- valFromObj "datavalue" snak
      datatype  <- valFromObj "datatype"  snak
      ass <- matchTypeFromJSON c [] datavalue datatype t
      return (VR ass)

dropURL s = match "http://www.wikidata.org/entity/" s
  where
    match [] ys = ys
    match (x : xs) (y : ys)
      | x == y = match xs ys
    match _ _ = s

decimal c ('+' : s) = decimal c s
decimal c s =
  case reads s of
    [(v, "")] -> return (c v)
    _         -> fail "Not a decimal"

int2digits abstr (VInt n)
  | n >= 0    = digits n
  | otherwise = VError (pp "Can't convert" <+> pp n)
  where
    digit n = vapp abstr ('D':'_':show n) []

    digits n =
      let (n2,n1) = divMod n 10
      in rest n2 (vapp abstr "IDig" [digit n1])

    rest 0 t = t
    rest n t =
      let (n2,n1) = divMod n 10
      in rest n2 (vapp abstr "IIDig" [digit n1, t])
int2digits abstr (VFV c vs) = VFV c (mapVariants (int2digits abstr) vs)

int2decimal :: ModuleName -> Value -> Value
int2decimal abstr (VInt n) = sign n (int2digits abstr (VInt (abs n)))
  where
    sign n t
      | n < 0     = vapp abstr "NegDecimal" [t]
      | otherwise = vapp abstr "PosDecimal" [t]
int2decimal abstr (VFV c vs) = VFV c (mapVariants (int2decimal abstr) vs)

float2decimal :: ModuleName -> Value -> Value
float2decimal abstr (VFlt f) =
  let n = truncate f
  in fractions (f-fromIntegral n) (int2decimal abstr (VInt n))
  where
    digit n = vapp abstr ('D':'_':show n) []

    fractions f t
      | f < 1e-8  = t
      | otherwise =
          let f10 = f * 10
              n2  = truncate f10
          in fractions (f10-fromIntegral n2) (vapp abstr "IFrac" [t, digit n2])
float2decimal abstr (VFV c vs) = VFV c (mapVariants (float2decimal abstr) vs)

int2numeral abstr (VInt n)
  | n < 1000000000000 = app1 "num" (n2s1000000000000 n)
  | otherwise         = range_error n
  where
    n2s1000000000000 n
      | n < 1000000000 = app1 "pot4as5" (n2s1000000000 n)
      | otherwise      = let (n1,n2) = divMod n 1000000000
                         in if n2 == 0
                            then app1 "pot5" (n2s1000 n1)
                            else app2 "pot5plus" (n2s1000 n1) (n2s1000000000 n2)

    n2s1000000000 n
      | n < 1000000 = app1 "pot3as4" (n2s1000000 n)
      | otherwise   = let (n1,n2) = divMod n 1000000
                      in if n2 == 0
                           then app1 "pot4" (n2s1000 n1)
                           else app2 "pot4plus" (n2s1000 n1) (n2s1000000 n2)

    n2s1000000 n
      | n < 1000  = app1 "pot2as3" (n2s1000 n)
      | otherwise = let (n1,n2) = divMod n 1000
                    in if n2 == 0
                         then app1 "pot3" (n2s1000 n1)
                         else app2 "pot3plus" (n2s1000 n1) (n2s1000 n2)

    n2s1000 n
      | n < 100   = app1 "pot1as2" (n2s100 n)
      | otherwise = let (n1,n2) = divMod n 100
                    in if n2 == 0
                         then app1 "pot2" (n2s10 n1)
                         else app2 "pot2plus" (n2s10 n1) (n2s100 n2)

    n2s100 n
      | n <  10   = app1 "pot0as1" (n2s10 n)
      | n == 10   = app0 "pot110"
      | n == 11   = app0 "pot111"
      | n <  20   = app1 "pot1to19" (n2d (n-10))
      | otherwise = let (n1,n2) = divMod n 10
                    in if n2 == 0
                         then app1 "pot1" (n2d n1)
                         else app2 "pot1plus" (n2d n1) (n2s10 n2)

    n2s10 n
      | n < 1     = range_error n
      | n == 1    = app0 "pot01"
      | otherwise = app1 "pot0" (n2d n)

    n2d n = app0 ('n':show n)

    range_error n = VError (pp n <+> pp "cannot be represented as a numeral")

    app0 fn = vapp abstr fn []
    app1 fn v1 = vapp abstr fn [v1]
    app2 fn v1 v2 = vapp abstr fn [v1,v2]
int2numeral abstr (VFV c vs) = VFV c (mapVariants (int2numeral abstr) vs)

time2adv abs_mn (VStr s) =
  case matchISO8601 s of
    Just (year,month,day) ->
          let y = vapp abs_mn "intYear" [VInt year]
              m = case month of
                    0  -> Nothing
                    1  -> Just (vapp abs_mn "january_Month" [])
                    2  -> Just (vapp abs_mn "february_Month" [])
                    3  -> Just (vapp abs_mn "march_Month" [])
                    4  -> Just (vapp abs_mn "april_Month" [])
                    5  -> Just (vapp abs_mn "may_Month" [])
                    6  -> Just (vapp abs_mn "june_Month" [])
                    7  -> Just (vapp abs_mn "july_Month" [])
                    8  -> Just (vapp abs_mn "august_Month" [])
                    9  -> Just (vapp abs_mn "september_Month" [])
                    10 -> Just (vapp abs_mn "october_Month" [])
                    11 -> Just (vapp abs_mn "november_Month" [])
                    12 -> Just (vapp abs_mn "december_Month" [])
                    _  -> Just matchError
              d = case day of
                    0  -> Nothing
                    _  -> Just (vapp abs_mn "intMonthday" [VInt day])
          in case (m,d) of
               (Just m,Just d)  -> vapp abs_mn "dayMonthYearAdv" [d, m, y]
               (Just m,Nothing) -> vapp abs_mn "monthYearAdv" [m, y]
               (Nothing,_)      -> vapp abs_mn "yearAdv" [y]
    Nothing -> matchError
  where
    matchError = VError (pp s <+> "is not a valid timestamp")

    matchISO8601 s =
      case s of
        ('+':s) -> date   1  s
        ('-':s) -> date (-1) s
        s       -> date   1  s
      where
        date era (y1:y2:y3:y4:'-':m1:m2:'-':d1:d2:_) = do
          year  <- pure 0 `digit` y1 `digit` y2 `digit` y3 `digit` y4
          month <- pure 0 `digit` m1 `digit` m2
          day   <- pure 0 `digit` d1 `digit` d2
          return (era*year,month,day)
        date era _ = Nothing

        digit r c
          | isDigit c = fmap (\x -> (x*10+(fromIntegral (ord c - ord '0')))) r
          | otherwise = Nothing
time2adv abs_mn (VFV c vs) = VFV c (mapVariants (time2adv abs_mn) vs)

value2int (VInt n) = Const n
value2int _        = RunTime

vapp m n vs = VApp poison (m, identS n) vs

toBool True  = vapp cPredef "True" []
toBool False = vapp cPredef "False" []

langs = [
  ("af", "ParseAfr"),
  ("ar", "ParseAra"),
  ("bg", "ParseBul"),
  ("ca", "ParseCat"),
  ("zh", "ParseChi"),
  ("nl", "ParseDut"),
  ("en", "ParseEng"),
  ("et", "ParseEst"),
  ("fi", "ParseFin"),
  ("fr", "ParseFre"),
  ("de", "ParseGer"),
  ("it", "ParseIta"),
  ("ko", "ParseKor"),
  ("mt", "ParseMlt"),
  ("pl", "ParsePol"),
  ("pt", "ParsePor"),
  ("ro", "ParseRon"),
  ("ru", "ParseRus"),
  ("sl", "ParseSlv"),
  ("so", "ParseSom"),
  ("es", "ParseSpa"),
  ("sw", "ParseSwa"),
  ("sv", "ParseSwe"),
  ("th", "ParseTha"),
  ("tr", "ParseTur"),
  ("zu", "ParseZul")
  ]
