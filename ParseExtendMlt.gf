concrete ParseExtendMlt of ParseExtend =
 CatMlt, ExtendMlt - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
                      AdvIsNPAP, DetNPFem, DetNPMasc, ExistCN,
                      PiedPipingQuestSlash, PiedPipingRelSlash],
 NumeralMlt - [num], PunctuationX ** open Prelude, ResMlt, GrammarMlt in {

lin UttAP p ap = { s = ap.s ! mkGenNum p.a.g p.a.n } ;
    UttVP ant pol p vp = {s = ant.s ++ pol.s ++ infVP vp ant.a Pos p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarMlt.RelNP ;
    ExtRelNP = GrammarMlt.RelNP ;

lin BareN2 n = n ;

}
