concrete ParseExtendHun of ParseExtend =
  CatHun,
  ExtendHun [TPastSimple, CompoundN, GenModNP, UseDAP, UseDAPMasc, UseDAPFem],
  NumeralHun - [num], PunctuationX **
  open Prelude, ResHun, NounHun in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin num x = x ;
lin ExtRelNP = NounHun.RelNP ;
lin gen_Quant = DefArt ;

}
