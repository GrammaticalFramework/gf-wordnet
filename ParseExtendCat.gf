concrete ParseExtendCat of ParseExtend = 
  ExtendCat - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralCat - [num], PunctuationX ** open Prelude, CommonRomance in {

lin UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat Sub1000000000 = {s : CardOrd => Str ; n : Number} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;


}
	
