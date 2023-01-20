concrete ParseExtendRus of ParseExtend =
  NumeralRus - [num], PunctuationX **
  open Prelude in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

}
