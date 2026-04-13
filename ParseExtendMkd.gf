concrete ParseExtendMkd of ParseExtend =
  ExtendMkd - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],  NumeralMkd - [num], PunctuationX ** open Predef, Prelude, ResMkd, GrammarMkd in {

lin gen_Quant = DefArt ;
lin UttAP  p ap  = {s = ap.s ! Indef ! p.a.g} ;
lin UttVP ant pol p vp = {
      s = ant.s ++ pol.s ++ "да" ++
          case pol.p of {
            Pos => [] ;
            Neg => "не"
          } ++
          vp.present ! Perfective ! genNum2num p.a.g ! p.a.p ++
          vp.compl ! p.a
      } ;
lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin num x = x ;

}
