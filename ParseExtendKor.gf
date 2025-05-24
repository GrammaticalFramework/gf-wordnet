concrete ParseExtendKor of ParseExtend =
  CatKor, ExtendKor [GivenName, MaleSurname, FemaleSurname, PlSurname, FullName],
  NumeralKor - [num], PunctuationX ** open Prelude, ResKor in {

lin UttAP  p ap  = {s = ap.compar ++ ap.s ! VF Plain Pos} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;


}
