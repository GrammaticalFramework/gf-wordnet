concrete ParseExtendMkd of ParseExtend =
  NumeralMkd - [num], PunctuationX ** open Predef, Prelude, GrammarMkd in {

lin gen_Quant = DefArt ;
lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin num x = x ;

}
