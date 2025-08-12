concrete ParseExtendHun of ParseExtend =
  NumeralHun - [num], PunctuationX **
  open Prelude, ResHun in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin num x = x ;

}
