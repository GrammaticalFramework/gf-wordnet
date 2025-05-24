concrete ParseExtendRon of ParseExtend =
  ExtendRon - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralRon - [num], PunctuationX **
 open Prelude, ResRon, GrammarRon in {

lin
    UttAP  p ap = {s = ap.s ! AF p.a.g p.a.n Indef ANomAcc} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin
    EmbedVP ant pol p vp = {
        s = ant.s ++ pol.s ++
            "sã" ++ flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! p.a) ++ conjVP vp p.a ++vp.comp ! p.a ++ vp.ext ! Pos
      } ;
      
    UttVP ant pol p vp = {
        s = ant.s ++ pol.s ++
            "sã"  ++ flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! p.a) ++ conjVP vp p.a ++vp.comp ! p.a ++ vp.ext ! Pos
      } ;

lin num = \d ->
 { s = \\cse => table { NCard g => \\f => d.s ! (NCard g) ! f ! indep  ;
                        NOrd g => \\f =>  let ss = d.s ! (NOrd g) ! f ! indep
                                                 in 
                                       case d.size of 
                                              { sg => (artDem g Sg cse) ++ ss ;
                                                _  => mkOrd ss g cse
                                               }
                       };
   sp =  \\cse => table { NCard g => \\f => d.s ! (NCard g) ! f ! attr  ;
                        NOrd g => \\f =>  let ss = d.s ! (NOrd g) ! f ! indep
                                                 in 
                                       case d.size of 
                                              { sg => (artDem g Sg cse) ++ ss ;
                                                _  => mkOrd ss g cse
                                               }
                       };
   size = d.size
 } ;

lin RelNP = GrammarRon.RelNP ;
    ExtRelNP = GrammarRon.RelNP ;

lin BareN2 n = n ;

}
