concrete ParseExtendTur of ParseExtend = 
  ExtendTur - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralTur - [num], PunctuationX ** open Prelude, ResTur in {

lin
    UttAP p ap = {s = ap.s ! p.a.n ! Nom} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = {s : CardOrd => Number => Case => Str ; n : Number} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;

}
