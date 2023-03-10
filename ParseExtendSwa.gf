concrete ParseExtendSwa of ParseExtend =
 CatSwa,
 NumeralSwa - [num], PunctuationX ** open Prelude, ResSwa, GrammarSwa in {

lin UttAP  p ap  = { s = ap.s ! getGender p.a ! getNumber p.a } ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarSwa.RelNP ;
    ExtRelNP = GrammarSwa.RelNP ;
    
lin BareN2 n = n ;

}
