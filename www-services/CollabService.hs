module CollabService(colabService) where

import PGF2
import PGF2.Collab
import Network.HTTP
import OpenSSL
import Text.JSON
import Control.Concurrent.MVar
import Control.Monad(forM_,foldM,mplus)
import Data.Word
import Data.List
import qualified Data.Map as Map
import UUID

colabService gr stateRef rq =
  case decodeRequest rq of
    Ok fn     -> fn
    Error msg -> return
                   (Response
                      { rspCode = 500
                      , rspReason = "Fail"
                      , rspHeaders = [Header HdrContentType "text/plain; charset=UTF8"]
                      , rspBody = msg
                      })
  where
    decodeRequest rq = do
      json <- decode (rqBody rq)
      typ  <- valFromObj "type" json
      case typ of
        "getDocument" -> do mb_doc_id <- fmap Just (valFromObj "doc_id" json) `mplus` pure Nothing
                            lang <- valFromObj "lang" json
                            case Map.lookup lang (languages gr) of
                              Just cnc -> return (getDocument mb_doc_id cnc)
                              Nothing  -> fail "Missing language"
        "pushUpdates" -> do doc_id  <- valFromObj "doc_id" json
                            version <- valFromObj "version" json
                            updates <- valFromObj "updates" json
                            cursor  <- valFromObj "cursor"  json
                            return (pushUpdates doc_id version updates cursor)
        "pullUpdates" -> do doc_id  <- valFromObj "doc_id" json
                            version <- valFromObj "version" json
                            return (pullUpdates doc_id version)

    getDocument mb_doc_id cnc = do
      doc_id <- case mb_doc_id of
                  Just doc_id -> return doc_id
                  _           -> newUUID
      state <- takeMVar stateRef
      doc@(Document content updates cursors,_) <-
            case Map.lookup doc_id state of
              Just doc -> return doc
              Nothing  -> do chart <- parseChart cnc (startCat gr) ""
                             return (Document chart [] [],[])
      text <- getParseChartText content
      putMVar stateRef (Map.insert doc_id doc state)
      let json = toJSObject [("doc_id",   showJSON doc_id)
                            ,("version",  showJSON (length updates))
                            ,("document", showJSON text)
                            ,("cursors",  showJSON cursors)
                            ]
      return
        (Response
           { rspCode = 200
           , rspReason = "OK"
           , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
           , rspBody = encode json
           })

    pushUpdates doc_id version updates cursor = do
      state <- takeMVar stateRef
      case Map.lookup doc_id state of
        Just (doc,pending) -> do
           (doc',updates',new_updates,new_cursors) <- apply version updates cursor doc
           if null updates'
             then return ()
             else forM_ pending $ \var -> do
                    putMVar var updates'
           putMVar stateRef (Map.insert doc_id (doc',[]) state)
           return
             (Response
                { rspCode = 200
                , rspReason = "OK"
                , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                , rspBody = encode (makeObj [("updates", showJSON new_updates)
                                            ,("cursors", showJSON new_cursors)
                                            ])
                })
        Nothing -> do
          putMVar stateRef state
          return
            (Response
                { rspCode = 400
                , rspReason = "Invalid Document Id"
                , rspHeaders = []
                , rspBody = ""
                })

    pullUpdates doc_id version = do
      state <- takeMVar stateRef
      case Map.lookup doc_id state of
        Just (doc@(Document text updates cursors),pending)
           | version < length updates ->
                do putMVar stateRef (Map.insert doc_id (doc,pending) state)
                   return
                     (Response
                        { rspCode = 200
                        , rspReason = "OK"
                        , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                        , rspBody = encode (makeObj [("updates", showJSON (drop version updates))
                                                    ,("cursors", showJSON cursors)
                                                    ])
                        })
           | otherwise ->
                do var <- newEmptyMVar
                   putMVar stateRef (Map.insert doc_id (doc,var:pending) state)
                   updates <- takeMVar var
                   return
                     (Response
                        { rspCode = 200
                        , rspReason = "OK"
                        , rspHeaders = [Header HdrContentType "application/json; charset=UTF8"]
                        , rspBody = encode (makeObj [("updates", showJSON updates)
                                                    ,("cursors", showJSON cursors)
                                                    ])
                        })
        Nothing -> do
          putMVar stateRef state
          return
            (Response
                { rspCode = 400
                , rspReason = "Invalid Document Id"
                , rspHeaders = []
                , rspBody = ""
                })

type ClientID = String
data Update   = Update ClientID [Change] deriving Show
data Cursor   = Cursor ClientID Int
data Document = Document ParseChart [Update] [Cursor]

instance JSON Update where
    readJSON (JSObject jsobj) = do
      clientID <- valFromObj "clientID" jsobj
      changes  <- valFromObj "changes"  jsobj
      return (Update clientID changes)
    readJSON _ = fail "Every update must be an object"
    
    showJSON (Update clientID changes) =
      makeObj [("clientID", showJSON clientID)
              ,("changes",  showJSON changes)
              ]

instance JSON Cursor where
    readJSON (JSObject jsobj) = do
      clientID <- valFromObj "clientID" jsobj
      pos      <- valFromObj "pos"      jsobj
      return (Cursor clientID pos)
    readJSON _ = fail "Every update must be an object"

    showJSON (Cursor clientID pos) =
      makeObj [("clientID", showJSON clientID)
              ,("pos",  showJSON pos)
              ]

instance JSON Change where
    readJSON (JSRational _ i)              = return (Skip (round i))
    readJSON (JSArray (JSRational _ i:ss)) = return (Change (round i) (intercalate "\n" [fromJSString s | JSString s <- ss]))
    readJSON json = fail "Invalid Change representation"

    showJSON (Skip i)      = showJSON (fromIntegral i :: Word)
    showJSON (Change i s)  = showJSON (showJSON (fromIntegral i :: Word) : [showJSON l | l <- split s])
      where
        split cs =
          case break (=='\n') cs of
            (xs,'\n':cs) -> xs : split cs
            (xs,[])      -> [xs]

apply :: Int -> [Update] -> Cursor -> Document -> IO (Document,[Update],[Update],[Cursor])
apply version new_updates cursor (Document content updates cursors) = do
  let updates0 = drop version updates
      (new_updates',updates0') = transform new_updates updates0
  forM_ new_updates' update
  return
     (Document content
               (updates++new_updates')
               cursors
     ,new_updates'
     ,updates0'
     ,cursors
     )
  where
    update (Update _ changes) =
      changeParseChartText content changes

transform :: [Update] -> [Update] -> ([Update],[Update])
transform  = mapAccumL (\us1 u2 -> swap (mapAccumL (flip transform1) u2 us1))
  where
    swap (x,y) = (y,x)

transform1 :: Update -> Update -> (Update,Update)
transform1 (Update cid1 as) (Update cid2 bs) =
  case loop as bs of
    (as,bs) -> (Update cid1 as, Update cid2 bs)
  where
    loop as            []            = (as,[])
    loop []            bs            = ([],bs)
    loop (Skip n : as) (Skip m : bs) =
      case compare n m of
        LT -> case loop as (Skip (m-n) : bs) of
                (as,bs) -> (skip n as,skip n bs)
        EQ -> case loop as bs of
                (as,bs) -> (skip n as,skip m bs)
        GT -> case loop (Skip (n-m) : as) bs of
                (as,bs) -> (skip m as,skip m bs)
    loop (Skip n : as) (Change m s2 : bs) =
      case compare n m of
        LT -> case loop as (Change (m-n) s2 : bs) of
                (as,bs) -> (skip (length s2) as,change n [] bs)
        EQ -> case loop as bs of
                (as,bs) -> (skip (length s2) as,change m s2 bs)
        GT -> case loop (Skip (n-m) : as) bs of
                (as,bs) -> (skip (length s2) as,change m s2 bs)
    loop (Change n s1 : as) (Skip m : bs) =
      case compare n m of
        LT -> case loop as (Skip (m-n) : bs) of
                (as,bs) -> (change n s1 as,skip (length s1) bs)
        EQ -> case loop as bs of
                (as,bs) -> (change n s1 as,skip (length s1) bs)
        GT -> case loop (Change (n-m) s1 : as) bs of
                (as,bs) -> (change m [] as,skip (length s1) bs)
    loop (Change n s1 : as) (Change m s2 : bs) =
      case compare n m of
        LT -> case loop as (Change (m-n) s2 : bs) of
                (as,bs) -> (change 0 s1 as,skip (length s1) bs)
        EQ -> case loop as bs of
                (as,bs) -> (skip (length s2) (change 0 s1 as),skip (length s1) (change 0 s2 bs))
        GT -> case loop (Change (n-m) s1 : as) bs of
                (as,bs) -> (skip (length s2) as,change 0 s2 bs)

    skip n (Skip m : as) = Skip (fromIntegral n+m) : as
    skip n           as  = Skip (fromIntegral n)   : as

    change n s (Change m s1 : as) = Change (n+m) (s++s1) : as
    change n s                as  = Change n     s       : as
