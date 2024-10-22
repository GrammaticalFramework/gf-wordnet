concrete ParseExtendIna of ParseExtend =
   NumeralIna - [num], PunctuationX **
open Prelude, GrammarIna in {

lin
    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
    TPastSimple = GrammarIna.TPast ;

}
