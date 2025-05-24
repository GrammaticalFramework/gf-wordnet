concrete ParseExtendSlv of ParseExtend =
  ExtendSlv - [iFem_Pron, youPolFem_Pron, youPolPlFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralSlv - [num], PunctuationX ** 
  open Prelude, ResSlv in {

lin
    UttAP p ap = {s = ap.s ! Indef ! inanimateGender p.a.g ! Nom ! p.a.n} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;

}
