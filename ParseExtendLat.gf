concrete ParseExtendLat of ParseExtend =
  ExtendLat [TPastSimple,PassVPSlash],
  NumeralLat - [num], PunctuationX **
  open Prelude, ResLat in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    UttAP p ap = ss (ap.s ! Ag p.pers.g p.pers.n Nom );

lin that_RP = IdRP ;

}
