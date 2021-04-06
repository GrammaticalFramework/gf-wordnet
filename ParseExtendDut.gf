concrete ParseExtendDut of ParseExtend = 
  ExtendDut - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralDut - [num], PunctuationX ** open Prelude, ResDut in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = {s : CardOrd => Str ; n : Number ; attr : Str} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;


}
	
