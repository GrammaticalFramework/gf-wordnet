concrete ParseExtendKor of ParseExtend =
  CatKor, NumeralKor - [num], PunctuationX ** open Prelude, ResKor in {

lin UttAP  p ap  = {s = ap.compar ++ ap.s ! VF Plain Pos} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = ResKor.Numeral ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;


}
