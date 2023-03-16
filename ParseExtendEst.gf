concrete ParseExtendEst of ParseExtend =
  ExtendEst - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralEst - [num], PunctuationX ** 
  open Prelude, ResEst, GrammarEst in {

lin UttAP  p ap  = {s = ap.s ! False ! NCase (complNumAgr p.a) Nom} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarEst.RelNP ;
    ExtRelNP = GrammarEst.RelNP ;

lin BareN2 n = n ;


}
