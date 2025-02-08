concrete ParseExtendMon of ParseExtend =
  NumeralMon - [num], PunctuationX **
  open Prelude, ResMon in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

}
