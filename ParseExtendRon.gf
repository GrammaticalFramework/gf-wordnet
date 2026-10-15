concrete ParseExtendRon of ParseExtend =
  ExtendRon - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralRon - [num], PunctuationX **
 open Prelude, ResRon, GrammarRon in {

lin
    UttAP  p ap = {s = ap.s ! AF p.a.g p.a.n Indef ANomAcc} ;
    UttVPS p vps = {s = vps.s ! Indic ! p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat CNN = {
    s1,s2 : NCase => Str ;
    r1,r2 : Agr => NCase => Str ;
    n1,n2,n : Number ;
    g1,g2 : Gender ;
    a : Animacy
    } ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\c => num1.s ! (agrGender cn1.g num1.n) ++ cn1.s ! num1.n ! Indef ! (convCase c) ;
      s2 = \\c => num2.s ! (agrGender cn2.g num2.n) ++ cn2.s ! num2.n ! Indef ! (convCase c) ;
      r1 = \\agr,c => cn1.s ! num1.n ! Def ! (convCase c) ++
                       reflPoss agr num1.n (agrGender cn1.g num1.n) ;
      r2 = \\agr,c => cn2.s ! num2.n ! Def ! (convCase c) ++
                       reflPoss agr num2.n (agrGender cn2.g num2.n) ;
      n1 = num1.n ; n2 = num2.n ; n = conjNumber num1.n num2.n ;
      g1 = agrGender cn1.g num1.n ; g2 = agrGender cn2.g num2.n ; a = cn1.a
      } ;

    DetCNN quant conj cnn = heavyNP {
      s = \\c => quant.s ! False ! cnn.n1 ! cnn.g1 ! (convCase c) ++
                  cnn.s1 ! c ++ conj.s1 ++ conj.s2 ++
                  quant.s ! False ! cnn.n2 ! cnn.g2 ! (convCase c) ++ cnn.s2 ! c ;
      a = agrP3 cnn.g1 cnn.n ; hasClit = HasRef False ;
      ss = cnn.s1 ! No ++ conj.s1 ++ conj.s2 ++ cnn.s2 ! No
      } ;

    ReflPossCNN conj cnn = {
      s = \\agr,c => cnn.r1 ! agr ! c ++ conj.s1 ++ conj.s2 ++ cnn.r2 ! agr ! c ;
      isPron = False
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,c => quant.s ! False ! cnn.n1 ! cnn.g1 ! (convCase c) ++ cnn.s1 ! c ++
                      "de" ++ rnp.s ! agr ! Ge ++ conj.s1 ++ conj.s2 ++
                      quant.s ! False ! cnn.n2 ! cnn.g2 ! (convCase c) ++ cnn.s2 ! c ++
                      "de" ++ rnp.s ! agr ! Ge ;
      isPron = False
      } ;

lin gen_Quant = DefArt ;

    ReflVPSlash = ExtendRon.ReflRNP ;
    ReflA2 = ExtendRon.ReflA2RNP ;
    RecipVPSlash slash = GrammarRon.ReflVP slash ;
    RecipVPSlashCN slash cn = GrammarRon.ReflVP slash ;

    NumLess n = n ** {s = \\g => n.s ! g ++ "mai puțini"} ;
    NumMore n = n ** {s = \\g => n.s ! g ++ "în plus"} ;

    UseACard ac = {s,sp = \\_ => ac.s ; n = Pl ; size = pl} ;
    UseAdAACard ada ac = {s,sp = \\_ => ada.s ++ ac.s ; n = Pl ; size = pl} ;

    ExtAdvAP ap adv = ap ** {s = \\af => ap.s ! af ++ SOFT_BIND ++ "," ++ adv.s} ;
    ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Masc Sg
      } ;
    CAdvAP pol cadv ap comp = ap ** {
      s = \\af => pol.s ++ cadv.s ++ ap.s ! af ++ cadv.p ++ comp.s ! agrP3 Masc Sg ;
      isPre = False
      } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.sNum ++ cadv.p} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\af => ap.s ! af ++ "destul pentru a" ++ ant.s ++ pol.s ++
                  vp.s ! Inf ++ vp.comp ! agrP3 Masc Sg ++ vp.ext ! pol.p ;
      isPre = False
      } ;
    EnoughAdv adv = {s = adv.s ++ "destul"} ;
    TimeNP np = {s = (np.s ! No).comp} ;
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

    whatSgFem_IP = whatSg_IP ** {a = aagr Fem Sg} ;
    whatSgNeut_IP = whatSg_IP ** {a = aagr Masc Sg} ;

lin
    EmbedVP ant pol p vp = {
        s = ant.s ++ pol.s ++
            "să" ++ flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! p.a) ++ conjVP vp p.a ++vp.comp ! p.a ++ vp.ext ! Pos
      } ;

    ComplVV vv ant pol vp =
      insertSimpObj
        (\\a => ant.s ++ "să" ++ vp.neg ! pol.p ++
               flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! a) ++
               conjVP vp a ++ vp.comp ! a ++ vp.ext ! pol.p)
        (useVerb vv) ;

    SlashVV vv ant pol vp =
      (insertSimpObj
        (\\a => ant.s ++ "să" ++ vp.neg ! pol.p ++
               flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! a) ++
               conjVP vp a ++ vp.comp ! a ++ vp.ext ! pol.p)
        (useVerb vv)) ** {
          c2 = vp.c2 ; needAgr = vp.needAgr ; needClit = vp.needClit ; lock_VPSlash = <>
        } ;

    SlashV2V v ant pol vp =
      (insertSimpObj
        (\\a => ant.s ++ "să" ++ vp.neg ! pol.p ++
               flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! a) ++
               conjVP vp a ++ vp.comp ! a ++ vp.ext ! pol.p)
        (useVerb v)) ** {
          c2 = v.c2 ; needAgr = True ; needClit = True ; lock_VPSlash = <>
        } ;

    SlashV2VNP v np ant pol vp =
      (insertSimpObj
        (\\a => v.c2.s ++ (np.s ! v.c2.c).comp ++ ant.s ++ "să" ++ vp.neg ! pol.p ++
               flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! a) ++
               conjVP vp a ++ vp.comp ! a ++ vp.ext ! pol.p)
        (useVerb v)) ** {
          c2 = vp.c2 ; needAgr = vp.needAgr ; needClit = vp.needClit ; lock_VPSlash = <>
        } ;

    InOrderToVP ant pol p vp = {
      s = "pentru a" ++ ant.s ++ pol.s ++ vp.s ! Inf ++ vp.comp ! p.a ++ vp.ext ! pol.p
      } ;
    CompVP ant pol p vp = {
      s = \\_ => "să" ++ ant.s ++ vp.neg ! pol.p ++
                  flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! p.a) ++
                  conjVP vp p.a ++ vp.comp ! p.a ++ vp.ext ! pol.p
      } ;
    FocusComp comp np =
      mkClause (comp.s ! np.a) False np.a
        (insertSimpObj (\\_ => (np.s ! No).comp) copula) ;
      
    UttVP ant pol p vp = {
        s = ant.s ++ pol.s ++
            "să"  ++ flattenSimpleClitics vp.nrClit vp.clAcc vp.clDat (vp.isRefl ! p.a) ++ conjVP vp p.a ++vp.comp ! p.a ++ vp.ext ! Pos
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

lin that_RP = IdRP ;

oper
    reflPoss : Agr -> Number -> Gender -> Str = \agr,n,g ->
      case <agr.n,agr.p> of {
        <Sg,P1> => i_Pron.poss ! n ! g ! ANomAcc ;
        <Sg,P2> => youSg_Pron.poss ! n ! g ! ANomAcc ;
        <Sg,P3> => he_Pron.poss ! n ! g ! ANomAcc ;
        <Pl,P1> => we_Pron.poss ! n ! g ! ANomAcc ;
        <Pl,P2> => youPl_Pron.poss ! n ! g ! ANomAcc ;
        <Pl,P3> => they_Pron.poss ! n ! g ! ANomAcc
        } ;

}
