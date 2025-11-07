concrete ParseExtendJpn of ParseExtend =
  NumeralJpn - [num], PunctuationX **
  open Prelude, ResJpn in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

}
