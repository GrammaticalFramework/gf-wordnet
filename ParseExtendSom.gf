concrete ParseExtendSom of ParseExtend =
  CatSom, NumeralSom - [num], PunctuationX ** open Prelude, ResSom in {

lin UttAP  p ap  = { s = ap.s ! AF (getNum p.a) Abs } ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = {
    s : DForm => Str ;
    thousand : Str ; -- TODO figure out if this really works so
    hasThousand : Bool ;
    ord : Str ;
    da : DefArticle ;
    n : Number
  } ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;


}
