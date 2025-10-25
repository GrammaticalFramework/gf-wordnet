concrete ParseExtendEus of ParseExtend =
  CatEus,
  NumeralEus - [num], PunctuationX **
  open Prelude, ResEus in {

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin UttAP _ ap = { s = ap.s ! Hau } ;
    
lin num x = x ;

}
