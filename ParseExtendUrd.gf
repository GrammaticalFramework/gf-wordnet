concrete ParseExtendUrd of ParseExtend =
  NumeralUrd - [num], PunctuationX **
  open Prelude, ResUrd in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

}
