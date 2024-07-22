concrete ParseExtendGer of ParseExtend =
  ExtendGer - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash, ReflA2RNP], NumeralGer - [num], PunctuationX **
 open Prelude, ResGer, (G=GrammarGer) in {

lin
    UttAP  p ap = {s = ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ++ ap.ext} ;
    UttVPS p vps= let vpss = vps.s ! Main ! p.a in {s = vpss.verb ++ vpss.compl} ; --- is the order as intended?

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin SlashV2VNP v np ant pol vp =   -- bitte ihn, zu kaufen | lasse ihn kaufen   HL 3/22
      let prep = v.c2 ;
          obj = appPrep prep (np.s!False) ; -- simplify: no glueing of prep+DefArt, HL 8/22
          b : Bool = case prep.t of {isPrep | isPrepDefArt => True ; _ => False} ;
          c = prep.c ;
          w = np.w ;
          vps = (ComplVV v ant pol vp ** {c2 = vp.c2 ; objCtrl = vp.objCtrl})
      in
      insertObj' obj b w c vps ;

    ComplVV v ant pol vp = -- HL 3/22: leave inf-complement in-place, extract infzu-complement
      let
        vps = predVGen v.isAux v ; -- e.g. will.isAux=True | wagt.isAux=False
        inf = mkInf v.isAux Simul pol.p vp
      in
      insertExtrapos vp.ext (insertInf inf vps) ;

lin num x = x ;

lin RelNP = G.RelNP ;
    ExtRelNP = G.RelNP ;

lin BareN2 n = n ;

}
