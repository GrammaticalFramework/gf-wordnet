concrete ParseExtendSwa of ParseExtend =
 CatSwa,
 NumeralSwa - [num], PunctuationX ** open Prelude, ResSwa in {

lin UttAP  p ap  = { s = ap.s ! getGender p.a ! getNumber p.a } ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat 
  Sub1000000000 = {s : CardOrd => Gender => Str ; n : Number} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;

}
