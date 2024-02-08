concrete ParseExtendAra of ParseExtend =
 CatAra,
 ExtendAra - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
 NumeralAra - [num],
 PunctuationX
  ** open Prelude, ResAra, GrammarAra, ParadigmsAra in {

lin
    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ! masc ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarAra.RelNP ;
    ExtRelNP = GrammarAra.RelNP ;

lin BareN2 n = n ;

}
