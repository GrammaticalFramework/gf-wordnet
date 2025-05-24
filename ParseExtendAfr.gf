concrete ParseExtendAfr of ParseExtend =
  CatAfr, 
  ExtendAfr [GN,SN,PN,GivenName, MaleSurname, FemaleSurname, PlSurname, FullName, PassVPSlash, PassAgentVPSlash, UseComp_estar, UseComp_ser, TPastSimple],
  NumeralAfr - [num], PunctuationX ** open Prelude, ResAfr, GrammarAfr in {

lin UttAP  p ap  = {s = ap.s ! APred} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarAfr.RelNP ;
    ExtRelNP = GrammarAfr.RelNP ;

lin BareN2 n = n ;


}
