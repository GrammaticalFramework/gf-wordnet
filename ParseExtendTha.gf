concrete ParseExtendTha of ParseExtend =
  NumeralTha - [num], PunctuationX **
  open Prelude, GrammarTha in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarTha.RelNP ;
    ExtRelNP = GrammarTha.RelNP ;

}
