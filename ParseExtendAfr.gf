concrete ParseExtendAfr of ParseExtend =
  CatAfr, NumeralAfr - [num], PunctuationX ** open Prelude, ResAfr in {

lin UttAP  p ap  = {s = ap.s ! APred} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = {s : CardOrd => Str ; n : Number ; attr : Str} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;


}
