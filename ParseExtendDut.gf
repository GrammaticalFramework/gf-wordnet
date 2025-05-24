concrete ParseExtendDut of ParseExtend =
  ExtendDut - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralDut - [num], PunctuationX ** 
  open Prelude, ResDut, GrammarDut in {

lin UttAP  p ap  = {s = ap.s ! p.a ! APred} ;
    UttVPS p vps = {s = vps.s ! Main ! p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin that_RP = IdRP ;

lin RelNP = GrammarDut.RelNP ;
    ExtRelNP = GrammarDut.RelNP ;

lin BareN2 n = n ;


}
