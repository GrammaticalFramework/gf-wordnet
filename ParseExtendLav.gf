concrete ParseExtendLav of ParseExtend =
  ExtendLav - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralLav - [num], PunctuationX **
  open Prelude, ResLav in {

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin UttAP p ap = { s = let a = fromAgr p.agr in ap.s ! Indef ! a.gend ! a.num ! Nom } ;

lin ComplVV vv ant pol vp = {
      v        = vv ;
      compl    = \\agr => ant.s ++ pol.s ++ buildVP vp pol.p VInf agr ;
      voice    = Act ;
      leftVal  = vv.leftVal ;
      rightAgr = AgrP3 Sg Masc ;
      rightPol = Pos ;
      objPron  = False
    } ;

    SlashV2V v2v ant pol vp = {
      v        = v2v ;
      compl    = \\agr => ant.s ++ pol.s ++ buildVP vp pol.p VInf agr ;
      voice    = Act ;
      leftVal  = v2v.leftVal ;
      rightAgr = AgrP3 Sg Masc ;
      rightPol = Pos ;
      objPron  = False ;  -- will be overriden
      rightVal = v2v.rightVal
    } ;

lin EmbedVP ant pol p vp = { s = ant.s ++ pol.s ++ buildVP vp pol.p VInf p.agr } ;

lin num x = x ;

lin that_RP = IdRP ;

}
