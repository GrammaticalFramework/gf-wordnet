concrete ParseExtendAfr of ParseExtend =
  CatAfr, NumeralAfr - [num], PunctuationX ** open Prelude, ResAfr in {

lin UttAP  p ap  = {s = ap.s ! APred} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;


}
