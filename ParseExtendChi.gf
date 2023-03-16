concrete ParseExtendChi of ParseExtend =
  ExtendChi - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP],
  NumeralChi - [num], PunctuationX **
  open Prelude, GrammarChi in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin ComplVV v ant pol vp = {
      verb = v ;
      compl = vp.prePart ++ vp.verb.s ++ vp.compl ;
      prePart, topic = [] ;
      isAdj = False ;
      } ;

lin num x = x ;

lin RelNP = GrammarChi.RelNP ;
    ExtRelNP = GrammarChi.RelNP ;

}
