concrete ParseExtendRus of ParseExtend =
  ExtendRus - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralRus - [num], PunctuationX **
  open ParamRus, GrammarRus, ParadigmsRus, ResRus, Prelude in {

lin UttAP p ap = {s = case p.a of {
                         Ag gn p => ap.s ! gn ! Animate ! Nom
                      }
                 } ;

    UttVP ant pol p vp = {
      s=ant.s ++ pol.s ++
        case pol.p of {
          Neg => "не" ;
          Pos => ""
        } ++
        vp.adv ! p.a ++ (verbInf vp.verb) ++ vp.dep ++ vp.compl ! Pos ! p.a
      } ;

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin that_RP = IdRP ;

lin RelNP = GrammarRus.RelNP ;
    ExtRelNP = GrammarRus.RelNP ;

}
