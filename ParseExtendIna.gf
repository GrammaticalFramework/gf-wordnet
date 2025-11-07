concrete ParseExtendIna of ParseExtend =
   NumeralIna - [num], PunctuationX **
open Prelude, GrammarIna in {

lin
    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
    TPastSimple = GrammarIna.TPast ;

lin that_RP = IdRP ;

}
