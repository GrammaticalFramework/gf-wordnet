concrete ParseExtendZul of ParseExtend =
  NumeralZul - [num], PunctuationX ** open Prelude in {

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

}
