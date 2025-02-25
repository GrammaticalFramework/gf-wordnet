{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase, MonadComprehensions #-}
module FunctionsService where

import Control.Applicative     (liftA2, (<|>))
import Control.Monad (MonadPlus(mplus), foldM, forM, msum, (>=>))
import GF.Compile
import GF.Compile.Compute.Concrete
import qualified GF.Data.ErrM            as E
import GF.Grammar              hiding (VApp, VRecType)
import GF.Grammar.Lookup
import GF.Infra.CheckM
import GF.Infra.Option
import GF.Term
import GF.Compile.Rename
import GF.Text.Pretty
import GF.Data.XML
import Network.HTTP
import Network.HTTP.MD5
import Network.URI
import OpenSSL
import PGF2
import System.IO ( utf8 )
import System.FilePath
import System.Directory ( doesFileExist )
import Text.JSON
import Text.JSON.Types (get_field, JSObject(..))
import Database.Daison
import SenseSchema

import qualified Data.ByteString.Char8   as BS
import qualified Data.Map                as Map
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.ByteString.Lazy as BL
import Data.Char ( isDigit, ord )
import Data.Foldable ( find )
import Data.Maybe
import Debug.Trace

import System.Environment ( getArgs )

main = do -- Testing driver: evaluates an expression given as an argument
  let doc_dir = ".."
  gr <- readNGF (doc_dir</>"Parse.ngf")
  db <- openDB (doc_dir</>"semantics.db")
  (_,(mn,sgr)) <- batchCompile noOptions (Just gr) [doc_dir</>"gf/WordNet.gf"]
  let qid  = "Q34"
      lang = "ParseEng"
      code = "expr qid"
  code <- fromMaybe "expr qid" . listToMaybe <$> getArgs
  res <- executeCode db gr sgr mn "." qid lang code
  putStrLn $ rspBody res

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
    path = uriPath (rqURI rq)

    parseQuery query = do
      qid  <- valFromObj "qid"  query
      lang <- valFromObj "lang" query
      code <- valFromObj "code" query
      return (executeCode db gr sgr mn "" qid lang code)

    orFail :: String -> Maybe a -> Result a
    orFail s = maybe (fail s) pure

    getFromQuery query = do
      qid <- orFail "No QID" $ lookup "qid" query
      lang <- orFail "No lang" $ lookup "lang" query
      code <- orFail "No code" $ lookup "code" query
      return $ executeCode db gr sgr mn "" qid lang code

executeCode :: Database -> PGF -> SourceGrammar -> ModuleName -> String -> String -> String -> String -> IO Response
executeCode db gr sgr mn cwd qid lang code =
  case runLangP NLG pNLG (BS.pack code) of
    Right prog ->
      case runCheck (checkComputeProg prog) of
        E.Ok ((headers,dataset),msg)
                   -> return (Response
                                { rspCode = 200
                                , rspReason = "OK"
                                , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                                , rspBody = encode $
                                              makeObj [("msg",showJSON msg),
                                                       ("headers",showJSON headers),
                                                       ("dataset",showJSON dataset)]
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

    Just cnc = Map.lookup lang (languages gr)

    instantiate t (Prod Implicit x ty1 ty2) = do
      (i,_) <- newResiduation []
      instantiate (App t (ImplArg (Meta i))) ty2
    instantiate t ty = return (t, ty)

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
                     msrc    = "<NLG module>",
                     mseqs   = Nothing,
                     jments  = jments
                   }
      nlg_m <- renameModule cwd sgr (nlg_mn, nlg_mi)
      
      infoss <- checkInModule cwd nlg_mi NoLoc empty $ topoSortJments2 nlg_m
      let sgr'    = prependModule sgr nlg_m
          globals = Gl sgr (wikiPredef db gr)
      nlg_m <- foldM (foldM (checkInfo (mflags nlg_mi) cwd globals)) nlg_m infoss

      let sgr' = prependModule sgr nlg_m
          globals' = Gl sgr' (wikiPredef db gr)
          qident = (nlg_mn,identS "main")
      
      term' <- lookupResDef  sgr' qident
      
      (_,res_ty') <- inferLType globals' (Q qident)
      (term,res_ty) <- inferLType globals' (App (App (Q qident) (K qid)) (K lang))

      checkWarn (ppTerm Unqualified 0 term')
      checkWarn (ppTerm Unqualified 0 term)

      ts <- runEvalM globals' $ do
        (term,res_ty) <- instantiate term res_ty
        res <- eval [] term []
        value2term False [] res
      -- TODO: probably want to return types post-unification?
      --       but in that case, what do you do if there are multiple variants?
      return (toHeaders res_ty, [toRecord res_ty t | t <- ts])

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
    toRecord ty             t       = [toCell ty t]

    toCells []            as = []
    toCells ((l,ty):lbls) as =
      case lookup l as of
        Just (_,t) -> toCell ty t : toCells lbls as
        Nothing    -> "?"         : toCells lbls as

    toCell (Sort s)  t
      | s == cStr =
          case toStr t of
            Just s  -> s
            Nothing -> render (ppTerm Unqualified 0 t)
    toCell (QC (m,c)) t
      | m == abs_mn    = linearize cnc (toExpr t)
    toCell (Q (m,c)) t
      | m == cPredef && c == identS "Markup"
                       = foldr showsXML "" (toXML t)
      | m == cPredef && c == identS "Time"
                       = case toStr t of
                           Just s  -> s
                           Nothing -> render (ppTerm Unqualified 0 t)
    toCell ty        t = render (ppTerm Unqualified 0 t)

    toExpr (App t1 t2) = EApp (toExpr t1) (toExpr t2)
    toExpr (QC (_,c))  = EFun (showIdent c)
    toExpr (EInt n)    = ELit (LInt n)
    toExpr (EFloat d)  = ELit (LFlt d)
    toExpr (ImplArg t) = EImplArg (toExpr t)
    toExpr (Meta i)    = EMeta i
    toExpr t           = case toStr t of
                           Just s  -> ELit (LStr s)
                           Nothing -> EMeta 0

    toStr (K s)        = Just s
    toStr (C t1 t2)    = do s1 <- toStr t1
                            s2 <- toStr t2
                            return (s1 ++ " " ++ s2)
    toStr (Glue t1 t2) = do s1 <- toStr t1
                            s2 <- toStr t2
                            return (s1 ++ s2)
    toStr _            = Nothing

    toXML (Markup tag as ts)
      | tag == identW = concatMap toXML ts
      | otherwise     = [Tag (showIdent tag) (map toAttr as) (concatMap toXML ts)]
    toXML t                  = case toStr t of
                                 Just s  -> [Data s]
                                 Nothing -> [Data (linearize cnc (toExpr t))]

    toAttr (id,t) =
      case toStr t of
        Just s  -> (showIdent id, s)
        Nothing -> (showIdent id, render (ppTerm Unqualified 0 t))

    checkInfo :: Options -> FilePath -> Globals -> SourceModule -> (Ident,Info) -> Check SourceModule
    checkInfo opts cwd globals sm (c,info) = checkInModule cwd (snd sm) NoLoc empty $ do
       case info of
         ResOper pty pde -> do
            (pty', pde') <- case (pty,pde) of
                (Just (L loct ty), Just (L locd de)) -> do
                     ty'     <- chIn loct "operation" $ do
                                   (ty,_) <- checkLType globals ty typeType
                                   normalForm globals ty
                     (de',_) <- chIn locd "operation" $
                                   checkLType globals de ty'
                     return (Just (L loct ty'), Just (L locd de'))
                (Nothing         , Just (L locd de)) -> do
                     (de',ty') <- chIn locd "operation" $
                                     inferLType globals de
                     return (Just (L locd ty'), Just (L locd de'))
                (Just (L loct ty), Nothing) -> do
                     chIn loct "operation" $
                        checkError (pp "No definition given to the operation")
            update sm c (ResOper pty' pde')
       where
         gr = prependModule sgr sm
         chIn loc cat = checkInModule cwd (snd sm) loc ("Happened in" <+> cat <+> c)

         update (mn,mi) c info = return (mn,mi{jments=Map.insert c info (jments mi)})

wikiPredef :: Database -> PGF -> PredefTable s
wikiPredef db pgf = Map.fromList
  [ (identS "entity", pdStandard 2 $\ \[typ,VStr qid] -> fetch typ qid >>= \v -> return (Const v))
  , (identS "int2digits", pdStandard 1 $\ \[VInt n] -> int2digits abstr n >>= \v -> return (Const v))
  , (identS "int2decimal", pdStandard 1 $\ \[VInt n] -> int2decimal abstr n >>= \v -> return (Const v))
  , (identS "float2decimal", pdStandard 1 $\ \[VFlt f] -> float2decimal abstr f >>= \v -> return (Const v))
  , (identS "int2numeral", pdStandard 1 $\ \[VInt n] -> int2numeral abstr n >>= \v -> return (Const v))
  , (identS "expr", pdStandard 2 $\ \[typ,x] ->
        case x of
          VStr qid -> get_expr qid >>= check typ
          _ -> error (showValue x)
    )
  , (identS "time2adv", pdStandard 1 $\ \[x] ->
        case x of
          VStr time -> fmap Const (time2adv abstr time)
          _ -> error (showValue x)
    )
  , (cLessInt, pdStandard 2 $\ \[v1,v2] -> return (fmap toBool (liftA2 (<) (value2int v1) (value2int v2))))
  ]
  where
    abstr = moduleNameS (abstractName pgf)

    fetch typ qid = do
      rsp <- unsafeIOToEvalM (simpleHTTP (getRequest ("https://www.wikidata.org/wiki/Special:EntityData/"++qid++".json")))
      case decode (rspBody rsp) >>= valFromObj "entities" >>= valFromObj qid >>= valFromObj "claims" of
        Ok obj    -> filterJsonFromType obj typ
        Error msg -> evalError (pp msg)

    value2expr (GF.Term.VApp (_,f) tnks) =
      foldM mkApp (EFun (showIdent f)) tnks
      where
        mkApp e1 tnk = do
          v  <- force tnk
          e2 <- value2expr v
          return (EApp e1 e2)
    
    get_expr qid = do
      res <- unsafeIOToEvalM $ 
               runDaison db ReadOnlyMode $
                 select [return (Const (VApp (abstr,identS (lex_fun lex)) [])) | (_,lex) <- fromIndex lexemes_qid (at qid)]
      msum res
    
    check ty t@(Const value) = do
      ty    <- value2term False [] ty
      value <- value2term False [] value
      EvalM $ \globals k mt b r msgs -> do
        let k' _ _ _ r' msgs' = return $ Success r' msgs'
        (case checkLType' value ty of EvalM f -> f globals k' mt b r msgs) >>= \case
          Success r' msgs' -> k t mt b r' msgs'
          Fail _ _ -> return $ Success r msgs
    check _ _ = return RunTime

filterJsonFromType :: JSObject [JSObject JSValue] -> Value s -> EvalM s (Value s)
filterJsonFromType obj typ =
  case typ of
   VRecType fields -> do fields <- mapM (getSpecificProperty obj) fields
                         return (VR fields)
   VMeta _ _       -> do fields <- getAllProperties obj
                         ty <- value2term False [] typ
                         t <- value2term False [] (VR fields)
                         (t,_) <- checkLType' t ty
                         eval [] t []
   _               -> evalError (pp "Wikidata entities are always records")

isProperty ('P':cs) = all isDigit cs
isProperty _        = False

getSpecificProperty :: JSObject [JSObject JSValue] -> (Label, Value s) -> EvalM s (Label, Thunk s)
getSpecificProperty obj (LIdent field, typ)
  | isProperty label =
      case Text.JSON.Types.get_field obj label of
        Nothing   -> do tnk <- newThunk [] (FV [])
                        return (LIdent field, tnk)
        Just objs -> do terms <- mapM (transformJsonToTerm typ) objs
                        tnk <- newThunk [] (FV terms)
                        return (LIdent field, tnk)
  | otherwise = evalError (pp field <+> "is an invalid Wikidata property")
  where
    label = showRawIdent field

    transformJsonToTerm :: Value s -> JSObject JSValue -> EvalM s Term
    transformJsonToTerm typ obj =
      case fromJSObjectToTerm obj typ of
        Ok ass    -> do typ <- value2term False [] typ
                        (t,_) <- checkLType' (R ass) typ
                        return t
        Error msg -> evalError (pp msg)
getSpecificProperty obj (LVar n, typ) =
  evalError (pp "Wikidata entities can only have named properties")

getAllProperties :: JSObject [JSObject JSValue] -> EvalM s [(Label, Thunk s)]
getAllProperties obj = fmap catMaybes $ forM (fromJSObject obj) $ \(label, objs) ->
  case mapM parseVariant objs of
    Ok ts     -> do tnk <- newThunk [] (FV ts)
                    return $ Just (LIdent (rawIdentS label), tnk)
    Error msg -> do evalWarn (pp msg)
                    return Nothing
  where
    parseVariant obj = do
      (qs, dv, dt) <- parseWikiDataProp obj
      wdt <- getWikiDataType dt
      fs <- forM (wdtFields wdt) $ \f ->
        assign (LIdent (rawIdentS (fieldName f))) <$> extractField f Nothing dv
      return $ R fs

fromJSObjectToTerm :: JSObject JSValue -> Value s -> Result [Assign]
fromJSObjectToTerm obj typ = do
  (qs, dv, dt) <- parseWikiDataProp obj
  matchTypeFromJSON qs dv dt typ

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
  , extractField :: forall s. Maybe (Value s) -> JSObject JSValue -> Result Term
  }
newtype WikiDataType = WikiDataType { wdtFields :: [WikiDataFieldType] }

valField :: JSON a => String -> GF.Grammar.Type -> (forall s. Maybe (Value s) -> a -> Result Term) -> WikiDataFieldType
valField n ty f = MkField n ty $ \ty -> valFromObj "value" >=> valFromObj n >=> f ty

valField' :: JSON a => String -> GF.Grammar.Type -> (a -> Term) -> WikiDataFieldType
valField' n ty f = valField n ty (const (pure . f))

matchTypeFromJSON qs dv dt (VRecType labels) = do
  wdt <- getWikiDataType dt
  traverse (getField wdt) labels
  where
    getField wdt (k@(LIdent l),ty) = assign k <$> let n = showRawIdent l in
      case find (\f -> fieldName f == n) (wdtFields wdt) of
        Just f  -> extractField f (Just ty) dv
        Nothing -> getQualifierOrReference qs dt l ty
    getField _ _ = fail "Wikidata entities can only have named properties"

getWikiDataType "commonsMedia"     = return commonsMediaWdt
getWikiDataType "quantity"         = return quantityWdt
getWikiDataType "wikibase-item"    = return wikibaseItemWdt
getWikiDataType "globe-coordinate" = return globeCoordinateWdt
getWikiDataType "time"             = return timeWdt
getWikiDataType "monolingualtext"  = return monolingualTextWdt
getWikiDataType dt                 = Error $ "Unknown WikiData type: " ++ dt

commonsMediaWdt = WikiDataType
  [ MkField "s" typeString $ \_ -> valFromObj "value" >=> \s -> return (K (constructImgUrl s))
  ]
  where
    constructImgUrl :: String -> String
    constructImgUrl img =
      let name = map (\c -> if c == ' ' then '_' else c) (unEscapeString img)
          h    = md5ss utf8 name    
      in "https://upload.wikimedia.org/wikipedia/commons/"++take 1 h++"/"++take 2 h++"/"++name

wikibaseItemWdt = WikiDataType
  [ valField' "id" typeString K
  ]

globeCoordinateWdt = WikiDataType
  [ valField' "latitude"  typeFloat EFloat
  , valField' "longitude" typeFloat EFloat
  , valField' "precision" typeFloat EFloat
  , valField' "altitude"  typeFloat EFloat
  , valField' "globe"     typeStr   K
  ]

quantityWdt = WikiDataType
  [ valField "amount" typeInt $ \case
      Just (VApp f [])
        | f == (cPredef,cInt)   -> valFromObj "value" >=> decimal EFloat
        | f == (cPredef,cFloat) -> valFromObj "value" >=> decimal EInt
        | otherwise             -> \_ -> fail "Not an Int or Float"
      _                         -> valFromObj "value" >=> decimal EInt
  , valField' "unit" typeString (K . dropURL)
  ]

cTime = identS "Time"

timeWdt = WikiDataType
  [ valField' "time"          (cnPredef cTime) K
  , valField' "precision"     typeInt          EInt
  , valField' "calendarmodel" typeString       (K . dropURL)
  ]

monolingualTextWdt = WikiDataType
  [ valField' "text"     typeString K
  , valField' "language" typeString K
  ]

getQualifierOrReference qs dt l t
  | isProperty label =
        case lookup label qs of
          Just snaks -> return (FV [value | snak <- snaks, Ok value <- [get_value snak]])
          Nothing    -> return (FV [])
  | otherwise = fail "An invalid Wikidata qualifier or reference"
  where
    label = showRawIdent l

    get_value snak = do
      datavalue <- valFromObj "datavalue" snak
      datatype  <- valFromObj "datatype"  snak
      ass <- matchTypeFromJSON [] datavalue datatype t
      return (R ass)

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

int2digits abstr n
  | n >= 0    = digits n
  | otherwise = evalError (pp "Can't convert" <+> pp n)
  where
    idig    = (abstr,identS "IDig")
    iidig   = (abstr,identS "IIDig")

    digit n = (VApp (abstr,identS ('D':'_':show n)) [])

    digits n = do
      let (n2,n1) = divMod n 10
      tnk <- newEvaluatedThunk (digit n1)
      rest n2 (VApp idig [tnk])

    rest 0 t = return t
    rest n t = do
      let (n2,n1) = divMod n 10
      tnk1 <- newEvaluatedThunk (digit n1)
      tnk2 <- newEvaluatedThunk t
      rest n2 (VApp iidig [tnk1, tnk2])

int2decimal :: ModuleName -> Integer -> EvalM s (Value s)
int2decimal abstr n = int2digits abstr (abs n) >>= sign n
  where
    neg_dec = (abstr,identS "NegDecimal")
    pos_dec = (abstr,identS "PosDecimal")

    sign n t = do
      tnk <- newEvaluatedThunk t
      if n < 0
        then return (VApp neg_dec [tnk])
        else return (VApp pos_dec [tnk])

float2decimal :: ModuleName -> Double -> EvalM s (Value s)
float2decimal abstr f =
  let n = truncate f
  in int2decimal abstr n >>= fractions (f-fromIntegral n)
  where
    ifrac = (abstr,identS "IFrac")

    digit n = (VApp (abstr,identS ('D':'_':show n)) [])

    fractions f t
      | f < 1e-8  = return t
      | otherwise = do
          let f10 = f * 10
              n2  = truncate f10
          tnk1 <- newEvaluatedThunk t
          tnk2 <- newEvaluatedThunk (digit n2)
          fractions (f10-fromIntegral n2) (VApp ifrac [tnk1, tnk2])

int2numeral abstr n
  | n < 1000000000000 = n2s1000000000000 n >>= app1 "num"
  | otherwise         = range_error n
  where
    n2s1000000000000 n
      | n < 1000000000 = n2s1000000000 n >>= app1 "pot4as5"
      | otherwise      = let (n1,n2) = divMod n 1000000000
                         in if n2 == 0
                            then n2s1000 n1 >>= app1 "pot5"
                            else do n1 <- n2s1000       n1
                                    n2 <- n2s1000000000 n2
                                    app2 "pot5plus" n1 n2

    n2s1000000000 n
      | n < 1000000 = n2s1000000 n >>= app1 "pot3as4"
      | otherwise   = let (n1,n2) = divMod n 1000000
                      in if n2 == 0
                           then n2s1000 n1 >>= app1 "pot4"
                           else do n1 <- n2s1000    n1
                                   n2 <- n2s1000000 n2
                                   app2 "pot4plus" n1 n2

    n2s1000000 n
      | n < 1000  = n2s1000 n >>= app1 "pot2as3"
      | otherwise = let (n1,n2) = divMod n 1000
                    in if n2 == 0
                         then n2s1000 n1 >>= app1 "pot3"
                         else do n1 <- n2s1000 n1
                                 n2 <- n2s1000 n2
                                 app2 "pot3plus" n1 n2

    n2s1000 n
      | n < 100   = n2s100 n >>= app1 "pot1as2"
      | otherwise = let (n1,n2) = divMod n 100
                    in if n2 == 0
                         then n2s10 n1 >>= app1 "pot2"
                         else do n1 <- n2s10  n1
                                 n2 <- n2s100 n2
                                 app2 "pot2plus" n1 n2

    n2s100 n
      | n <  10   = n2s10 n >>= app1 "pot0as1"
      | n == 10   = return (VApp (abstr,identS "pot110") [])
      | n == 11   = return (VApp (abstr,identS "pot111") [])
      | n <  20   = n2d (n-10) >>= app1 "pot1to19"
      | otherwise = let (n1,n2) = divMod n 10
                    in if n2 == 0
                         then n2d n1 >>= app1 "pot1"
                         else do n1 <- n2d   n1
                                 n2 <- n2s10 n2
                                 app2 "pot1plus" n1 n2

    n2s10 n
      | n < 1     = range_error n
      | n == 1    = return (VApp (abstr,identS "pot01") [])
      | otherwise = n2d n >>= app1 "pot0"

    n2d n = app0 ('n':show n)

    range_error n = evalError (pp n <+> pp "cannot be represented as a numeral")

    app0 fn = return (VApp (abstr,identS fn) [])

    app1 fn v1 = do
      tnk1 <- newEvaluatedThunk v1
      return (VApp (abstr,identS fn) [tnk1])

    app2 fn v1 v2 = do
      tnk1 <- newEvaluatedThunk v1
      tnk2 <- newEvaluatedThunk v2
      return (VApp (abstr,identS fn) [tnk1,tnk2])

time2adv abs_mn s =
  case matchISO8601 s of
    Just (year,month,day) -> do
          y <- do y <- newEvaluatedThunk $ VInt year
                  newEvaluatedThunk $ VApp (abs_mn,identS "intYear") [y]
          m <- case month of
                 0  -> return Nothing
                 1  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "january_Month") [])
                 2  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "february_Month") [])
                 3  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "march_Month") [])
                 4  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "april_Month") [])
                 5  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "may_Month") [])
                 6  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "june_Month") [])
                 7  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "july_Month") [])
                 8  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "august_Month") [])
                 9  -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "september_Month") [])
                 10 -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "october_Month") [])
                 11 -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "november_Month") [])
                 12 -> fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "december_Month") [])
                 _  -> matchError
          d <- case day of
                 0  -> return Nothing
                 _  -> do d <- newEvaluatedThunk (VInt day)
                          fmap Just (newEvaluatedThunk $ VApp (abs_mn,identS "intMonthday") [d])
          case (m,d) of
            (Just m,Just d)  -> return $ VApp (abs_mn,identS "dayMonthYearAdv") [d, m, y]
            (Just m,Nothing) -> return $ VApp (abs_mn,identS "monthYearAdv") [m, y]
            (Nothing,_)      -> return $ VApp (abs_mn,identS "yearAdv") [y]
    Nothing -> matchError
  where
    matchError = evalError (pp s <+> "is not a valid timestamp")

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


value2int (VInt n) = Const n
value2int _        = RunTime

toBool True  = VApp (cPredef,identS "True")  []
toBool False = VApp (cPredef,identS "False") []

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
