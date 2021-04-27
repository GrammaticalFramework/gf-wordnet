concrete ParseExtendGer of ParseExtend =
  ExtendGer - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralGer - [num], PunctuationX **
 open Prelude, ResGer in {

lin
    UttAP  p ap = {s = ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ++ ap.ext} ;
    UttVPS p vps= {s = vps.s ! Main ! p.a} ;

    PhrUttMark pconj utt voc mark = {s = mark.s ++ SOFT_BIND ++ pconj.s ++ utt.s ++ voc.s} ;

lincat Sub1000000000 = {s : CardOrd => Str ; n : Number} ;

lin pot3as4 n = n ;

    num x = x ;

lin BareN2 n = n ;

}
