concrete ParseExtendSlv of ParseExtend = 
  ExtendSlv - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralSlv - [num], PunctuationX ** open Prelude, ResSlv in {

lin
    UttAP p ap = {s = ap.s ! Indef ! inanimateGender p.a.g ! Nom ! p.a.n} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = {s : Gender => Case => Str; n : NumAgr} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;


}
	
