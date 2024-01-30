concrete ParseExtendAra of ParseExtend =
 CatAra,
 NumeralAra - [num],
 PunctuationX
  ** open Prelude, ResAra, GrammarAra, ParadigmsAra in {

lin
    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ! masc ++ voc.s ++ SOFT_BIND ++ mark.s} ;

---lin num x = x ;

lin RelNP = GrammarAra.RelNP ;
    ExtRelNP = GrammarAra.RelNP ;

lin BareN2 n = n ;

}
