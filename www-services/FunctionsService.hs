{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase, MonadComprehensions #-}
module FunctionsService where

import Control.Applicative     (liftA2, (<|>))
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
        E.Ok (res,msg)
                   -> return (Response
                                { rspCode = 200
                                , rspReason = "OK"
                                , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                                , rspBody = encode $
                                              makeObj [("msg",showJSON msg)
                                                      ,("groups", showJSON [makeObj [("headers",showJSON headers),
                                                                                     ("dataset",showJSON dataset)]
                                                                              | (headers,dataset) <- res])
                                                      ]
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
                     msrc    = "<NLG module>",
                     mseqs   = Nothing,
                     jments  = jments
                   }
      nlg_m <- renameModule cwd sgr (nlg_mn, nlg_mi)
      
      infoss <- checkInModule cwd nlg_mi NoLoc empty $ topoSortJments2 nlg_m
      let sgr'     = prependModule sgr nlg_m
          globals0 = Gl sgr (wikiPredef db gr qid lang)
      nlg_m <- foldM (foldM (checkInfo (mflags nlg_mi) cwd globals0)) nlg_m infoss
      checkWarn (ppModule Unqualified nlg_m)

      let sgr' = prependModule sgr nlg_m
          globals1 = Gl sgr' (wikiPredef db gr qid lang)
          qident = (nlg_mn,identS "main")

      res <- runEvalM globals1 $ do
        g <- globals
        let (c1,c2) = split unit
        (term,res_ty) <- inferLType' (Q qident)
        (flag,term,res_ty) <- instantiate False term res_ty
        res <- value2termM True [] (eval g [] c2 term [])
        (res,res_ty) <- if flag
                          then inferLType' res
                          else return (res,res_ty)
        res_ty <- value2termM False [] res_ty
        return (toHeaders res_ty,[toRecord res_ty res])
      return ((Map.toList . Map.fromListWith (++)) res)

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
      | m == abs_mn = linearize cnc (toExpr t)
    toCell (Q (m,c)) t
      | m == cPredef && c == identS "Markup"
                       = foldr showsXML "" (toXML t)
      | m == cPredef && c == identS "Time"
                       = case toStr t of
                           Just s  -> s
                           Nothing -> render (ppTerm Unqualified 0 t)
    toCell ty        t = render (ppTerm Unqualified 0 t)

    toExpr (App t1 t2) = EApp (toExpr t1) (toExpr t2)
    toExpr (Q (_,c))   = EFun (showIdent c)
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

wikiPredef :: Database -> PGF -> String -> String -> PredefTable
wikiPredef db pgf qid lang = Map.fromList
  [ (identS "entity", pdArity 2 $ \g c [typ,VStr qid] -> Const (fetch typ qid))
  , (identS "int2digits", pdArity 1 $ \g c [VInt n] -> Const (int2digits abstr n))
  , (identS "int2decimal", pdArity 1 $ \g c [VInt n] -> Const (int2decimal abstr n))
  , (identS "float2decimal", pdArity 1 $ \g c [VFlt f] -> Const (float2decimal abstr f))
  , (identS "int2numeral", pdArity 1 $ \g c [VInt n] -> Const (int2numeral abstr n))
  , (identS "expr", pdArity 2 $ \g c [ty,x] ->
        case x of
          VStr qid -> get_expr c ty qid
          _        -> Const (VError ("2" <+> ppValue Unqualified 0 ty <+> ppValue Unqualified 0 x))
    )
  , (identS "time2adv", pdArity 1 $ \g c [x] ->
        case x of
          VStr time -> Const (time2adv abstr time)
          _         -> Const (VError (ppValue Unqualified 0 x))
    )
  , (identS "qid",  pdArity 0 $ \g c [] -> Const (VStr qid))
  , (identS "lang", pdArity 0 $ \g c [] -> Const (VStr lang))
  , (cLessInt, pdArity 2 $ \g c [v1,v2] -> fmap toBool (liftA2 (<) (value2int v1) (value2int v2)))
  ]
  where
    abstr = moduleNameS (abstractName pgf)

    fetch typ qid =
      let rsp = unsafePerformIO (simpleHTTP (getRequest ("https://www.wikidata.org/wiki/Special:EntityData/"++qid++".json")))
      in case decode (rspBody rsp) >>= valFromObj "entities" >>= valFromObj qid >>= valFromObj "claims" of
           Ok obj    -> undefined (obj :: JSValue) -- filterJsonFromType obj typ
           Error msg -> VError (pp msg)

    get_expr c ty qid = Const (VFV c res)
      where
        res = unsafePerformIO $
                runDaison db ReadOnlyMode $
                  select [VApp (abstr,identS id) [] | (_,lex) <- fromIndex lexemes_qid (at qid)
                                                    , let id = lex_fun lex
                                                    , fmap (matchType ty) (functionType pgf id) == Just True]

        matchType (VProd bt1 _ ty11 ty2) (DTyp ((bt2,_,ty12):hypos) cat2 []) =
          bt1 == bt2 && matchType ty11 ty12 && matchType ty2 (DTyp hypos cat2 [])
        matchType (VApp (mod,cat1) []) (DTyp [] cat2 []) =
          mod == abstr && showIdent cat1 == cat2
        matchType (VMeta _ _) _ = True
        matchType _ _ = False

{-
filterJsonFromType :: JSObject [JSObject JSValue] -> Value -> EvalM Value
filterJsonFromType obj typ =
  case typ of
   VRecType fields -> do fields <- mapM (getSpecificProperty obj) fields
                         return (VR fields)
   VMeta _ _       -> do fields <- getAllProperties obj
                         t <- value2term False [] (VR fields)
                         t <- checkLType' t typ
                         eval [] t []
   _               -> evalError (pp "Wikidata entities are always records")
-}
isProperty ('P':cs) = all isDigit cs
isProperty _        = False
{-
getSpecificProperty :: JSObject [JSObject JSValue] -> (Label, Value) -> EvalM (Label, Value)
getSpecificProperty obj (LIdent field, typ)
  | isProperty label =
      case Text.JSON.Types.get_field obj label of
        Nothing   -> do return (LIdent field, FV [])
        Just objs -> do terms <- mapM (transformJsonToTerm typ) objs
                        return (LIdent field, FV terms)
  | otherwise = evalError (pp field <+> "is an invalid Wikidata property")
  where
    label = showRawIdent field

    transformJsonToTerm :: Value -> JSObject JSValue -> EvalM Term
    transformJsonToTerm typ obj =
      case fromJSObjectToTerm obj typ of
        Ok ass    -> checkLType' (R ass) typ
        Error msg -> evalError (pp msg)
getSpecificProperty obj (LVar n, typ) =
  evalError (pp "Wikidata entities can only have named properties")

getAllProperties :: JSObject [JSObject JSValue] -> Choice -> [(Label, Value)]
getAllProperties c obj =
  flip mapMaybe (fromJSObject obj) $ \(label, objs) ->
    case mapM parseVariant objs of
      Ok ts     -> Just (LIdent (rawIdentS label), VFV c ts)
      Error msg -> Nothing
  where
    parseVariant obj = do
      (qs, dv, dt) <- parseWikiDataProp obj
      wdt <- getWikiDataType dt
      fs <- forM (wdtFields wdt) $ \f ->
        assign (LIdent (rawIdentS (fieldName f))) <$> extractField f Nothing dv
      return $ R fs

fromJSObjectToTerm :: JSObject JSValue -> Value -> Result [Assign]
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
  , extractField :: Maybe Value -> JSObject JSValue -> Choice -> Result Value
  }
newtype WikiDataType = WikiDataType { wdtFields :: [WikiDataFieldType] }

valField :: JSON a => String -> GF.Grammar.Type -> (Maybe Value -> a -> Result Term) -> WikiDataFieldType
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
  [ MkField "s" typeString $ \_ -> valFromObj "value" >=> \s -> return (VStr (constructImgUrl s))
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

getQualifierOrReference qs dt l t c
  | isProperty label =
        case lookup label qs of
          Just snaks -> return (VFV c [value | snak <- snaks, Ok value <- [get_value snak]])
          Nothing    -> return (VFV c [])
  | otherwise = fail "An invalid Wikidata qualifier or reference"
  where
    label = showRawIdent l

    get_value snak = do
      datavalue <- valFromObj "datavalue" snak
      datatype  <- valFromObj "datatype"  snak
      ass <- matchTypeFromJSON [] datavalue datatype t
      return (R ass)
-}
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
  | otherwise = VError (pp "Can't convert" <+> pp n)
  where
    idig    = (abstr,identS "IDig")
    iidig   = (abstr,identS "IIDig")

    digit n = VApp (abstr,identS ('D':'_':show n)) []

    digits n =
      let (n2,n1) = divMod n 10
      in rest n2 (VApp idig [digit n1])

    rest 0 t = t
    rest n t =
      let (n2,n1) = divMod n 10
      in rest n2 (VApp iidig [digit n1, t])

int2decimal :: ModuleName -> Integer -> Value
int2decimal abstr n = sign n (int2digits abstr (abs n))
  where
    neg_dec = (abstr,identS "NegDecimal")
    pos_dec = (abstr,identS "PosDecimal")

    sign n t
      | n < 0     = VApp neg_dec [t]
      | otherwise = VApp pos_dec [t]

float2decimal :: ModuleName -> Double -> Value
float2decimal abstr f =
  let n = truncate f
  in fractions (f-fromIntegral n) (int2decimal abstr n)
  where
    ifrac = (abstr,identS "IFrac")

    digit n = (VApp (abstr,identS ('D':'_':show n)) [])

    fractions f t
      | f < 1e-8  = t
      | otherwise =
          let f10 = f * 10
              n2  = truncate f10
          in fractions (f10-fromIntegral n2) (VApp ifrac [t, (digit n2)])

int2numeral abstr n
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

    app0 fn = VApp (abstr,identS fn) []
    app1 fn v1 = VApp (abstr,identS fn) [v1]
    app2 fn v1 v2 = VApp (abstr,identS fn) [v1,v2]

time2adv abs_mn s =
  case matchISO8601 s of
    Just (year,month,day) ->
          let y = VApp (abs_mn,identS "intYear") [VInt year]
              m = case month of
                    0  -> Nothing
                    1  -> Just (VApp (abs_mn,identS "january_Month") [])
                    2  -> Just (VApp (abs_mn,identS "february_Month") [])
                    3  -> Just (VApp (abs_mn,identS "march_Month") [])
                    4  -> Just (VApp (abs_mn,identS "april_Month") [])
                    5  -> Just (VApp (abs_mn,identS "may_Month") [])
                    6  -> Just (VApp (abs_mn,identS "june_Month") [])
                    7  -> Just (VApp (abs_mn,identS "july_Month") [])
                    8  -> Just (VApp (abs_mn,identS "august_Month") [])
                    9  -> Just (VApp (abs_mn,identS "september_Month") [])
                    10 -> Just (VApp (abs_mn,identS "october_Month") [])
                    11 -> Just (VApp (abs_mn,identS "november_Month") [])
                    12 -> Just (VApp (abs_mn,identS "december_Month") [])
                    _  -> Just matchError
              d = case day of
                    0  -> Nothing
                    _  -> Just (VApp (abs_mn,identS "intMonthday") [VInt day])
          in case (m,d) of
               (Just m,Just d)  -> VApp (abs_mn,identS "dayMonthYearAdv") [d, m, y]
               (Just m,Nothing) -> VApp (abs_mn,identS "monthYearAdv") [m, y]
               (Nothing,_)      -> VApp (abs_mn,identS "yearAdv") [y]
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
