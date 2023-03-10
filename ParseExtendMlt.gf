concrete ParseExtendMlt of ParseExtend =
 CatMlt,
 NumeralMlt - [num], PunctuationX ** open Prelude, ResMlt, GrammarMlt in {

lin UttAP  p ap  = { s = ap.s ! mkGenNum p.a.g p.a.n } ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarMlt.RelNP ;
    ExtRelNP = GrammarMlt.RelNP ;

lin BareN2 n = n ;

}
