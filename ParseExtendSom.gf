concrete ParseExtendSom of ParseExtend =
  CatSom, ExtendSom [GivenName, MaleSurname, FemaleSurname, PlSurname, FullName],
  NumeralSom - [num], PunctuationX ** open Prelude, ResSom, GrammarSom in {

lin UttAP  p ap  = { s = ap.s ! AF (getNum p.a) Abs } ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;

lin RelNP = GrammarSom.RelNP ;
    ExtRelNP = GrammarSom.RelNP ;

}
