concrete ParseExtendSpa of ParseExtend = 
  ExtendSpa - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralSpa - [num], PunctuationSpa **
 open Prelude in {

lin PhrUttMark pconj utt voc mark = {s = mark.s1 ++ SOFT_BIND ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s2} ;

}
