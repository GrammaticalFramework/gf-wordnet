import SG
import PGF2
import SenseSchema
import qualified Data.Map as Map
import Control.Monad(foldM)
import Text.JSON

main = do
  db <- openSG "semantics.db"
  let lex_ids = [lex_id | Just lex_id <- [readExpr "apple_1_N", readExpr "book_1_N"]]
  senses <- foldM (getSense db) Map.empty lex_ids
  putStrLn (encode (showJSON (map mkSenseObj (Map.toList senses))))
  closeSG db
  where
    mkSenseObj (sense_id,(mb_gloss,lex_ids)) =
      makeObj ([("sense_id",showJSON sense_id)]++
               maybe [] (\gloss->[("gloss",showJSON gloss)]) mb_gloss++
               [("lex_ids",showJSON (map (showExpr []) lex_ids))])

    getSense db senses lex_id = do
      res <- queryTriple db (Just lex_id) (Just synset) Nothing
      foldM (getGloss db) senses res

    getGloss db senses x@(_,lex_id,_,sense_id_e) = 
      case Map.lookup sense_id senses of
        Just (mb_gloss,lex_ids) -> return (Map.insert sense_id (mb_gloss,lex_id:lex_ids) senses)
        Nothing                 -> do res <- queryTriple db (Just sense_id_e) (Just gloss) Nothing
                                      let mb_gloss = head ([unStr gloss_str | (_,_,_,gloss_str) <- res]++[Nothing])
                                      return (Map.insert sense_id (mb_gloss,[lex_id]) senses)
      where
        Just (sense_id,[]) = unApp sense_id_e
