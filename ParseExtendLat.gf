concrete ParseExtendLat of ParseExtend =
  NumeralLat - [num], PunctuationX **
  open Prelude, ResLat in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

}
