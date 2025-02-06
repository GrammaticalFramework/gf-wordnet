concrete ParseExtendHin of ParseExtend =
  NumeralHin - [num], PunctuationX **
  open Prelude, ResHin in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

}
