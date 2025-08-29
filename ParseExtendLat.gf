concrete ParseExtendLat of ParseExtend =
  ExtendLat [TPastSimple,PassVPSlash],
  NumeralLat - [num], PunctuationX **
  open Prelude, ResLat in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

}
