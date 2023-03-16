concrete ParseExtendTur of ParseExtend =
  ExtendTur - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralTur - [num], PunctuationX ** 
  open Prelude, ResTur, GrammarTur in {

lin
    UttAP p ap = {s = ap.s ! p.a.n ! Nom} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;

lin RelNP = GrammarTur.RelNP ;
    ExtRelNP = GrammarTur.RelNP ;

}
