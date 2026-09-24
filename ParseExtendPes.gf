--# -path=.:../gf-rgl/src/persian:../gf-rgl/src/abstract:../gf-rgl/src/common:../gf-rgl/src/prelude

concrete ParseExtendPes of ParseExtend =
  ExtendPes - [
    iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
    GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
    CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
    UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
    PiedPipingQuestSlash, PiedPipingRelSlash
  ],
  NumeralPes - [num],
  PunctuationX - [QuestMark] ** open Prelude, ResPes in {

  lincat CNN = {s1 : Str ; s2 : Mod => Str ; n : Number} ;

  lin
    QuestMark = {s = "؟"} ;

    PhrUttMark pconj utt voc mark =
      {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    that_RP = {
      s = table {Ke => "که" ; Ance => "آنچه"} ;
      a = RNoAg
      } ;
    whatSgFem_IP = {s = "چه چیزی" ; n = Sg} ;
    whatSgNeut_IP = {s = "چه چیزی" ; n = Sg} ;

    num numeral = numeral ;
    UseACard card = {s = card.s ; n = Pl} ;
    UseAdAACard ada card = {s = ada.s ++ card.s ; n = Pl} ;
    NumLess number = number ** {s = number.s ++ "کمتر"} ;
    NumMore number = number ** {s = number.s ++ "بیشتر"} ;

    gen_Quant = makeQuant [] [] Bare False ** {isDef = False} ;

    UttAP pron ap = {
      s = pron.s ++ ap.s ! Bare ++ beVerb.s ! VAor Pos pron.a
      } ;
    UttVPS pron vps = {s = pron.s ++ vps.s ! pron.a} ;
    UttVP ant pol pron vp = {s = pron.s ++ finiteVP ant pol pron.a vp} ;

    TimeNP np = {s = np2str np} ;
    AdvAdv first second = {s = first.s ++ second.s} ;
    ExtAdvAP ap adv = ap ** {
      s = \\m => ap.s ! m ++ SOFT_BIND ++ "،" ++ adv.s ;
      adv = ap.adv ++ SOFT_BIND ++ "،" ++ adv.s
      } ;
    BareN2 noun = noun ;

    RelNP np rs = np ** {
      s = \\m => np.s ! Clitic ++ rs2str Ke np.a rs
      } ;
    ExtRelNP np rs = np ** {
      s = \\m => np.s ! Clitic ++ "،" ++ rs2str Ke np.a rs
      } ;

    ComparAdv pol cadv adv comp =
      {s = pol.s ++ adv.s ++ cadv.p ++ cadv.s ++ comp.s ! defaultAgr} ;
    CAdvAP pol cadv ap comp = ap ** {
      s = \\m => pol.s ++ cadv.s ++ comp.s ! defaultAgr ++ ap.s ! m ;
      adv = pol.s ++ cadv.s ++ comp.s ! defaultAgr ++ ap.adv
      } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ "از"} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\m => ap.s ! m ++ "به اندازه کافی" ++ finiteVP ant pol defaultAgr vp ;
      adv = ap.adv ++ "به اندازه کافی" ++ finiteVP ant pol defaultAgr vp
      } ;
    EnoughAdv adv = {s = adv.s ++ "به اندازه کافی"} ;

    ComplVV vv ant pol vp = predV vv ** {
      vComp = \\agr,tense => complVVAnt vv ant pol vp agr tense ;
      vvtype = case vv.isDef of {True => DefVV ; False => FullVV}
      } ;
    SlashVV vv ant pol slash = slash ** ComplVV vv ant pol slash ;
    EmbedVP ant pol pron vp = {s = "به" ++ infVP vp} ;
    SlashV2V verb ant pol vp = predVc verb ** {
      agrObj = \\agr => complVVAnt verb ant pol vp agr VVPres ;
      c2 = verb.c2
      } ;
    SlashV2VNP verb np ant pol slash = predVc verb ** {
      comp = \\_,wo => appCompVP verb.c2 np.s ! wo ;
      vComp = \\_,tense => complVVAnt verb ant pol slash np.a tense ;
      c2 = slash.c2
      } ;

    InOrderToVP ant pol pron vp =
      {s = "برای اینکه" ++ pron.s ++ finiteVP ant pol pron.a vp} ;
    CompVP ant pol pron vp =
      {s = \\_ => pron.s ++ finiteVP ant pol pron.a vp} ;

    ReflVPSlash slash rnp = insertCompPre (\\_ => rnp.s) slash ;
    ReflA2 adjective rnp = adjective ** {
      s = \\m => rnp.s ! Bare ++ adjective.c2 ++ adjective.s ! Positive ! m ;
      adv = rnp.s ! Bare ++ adjective.c2 ++ adjective.adv
      } ;
    RecipVPSlash slash = insertCompPre (\\_ => reciprocalNP.s) slash ;
    RecipVPSlashCN slash cn =
      insertCompPre (\\_ => \\_ => cn.s ! Sg ! Ezafe ++ "یکدیگر") slash ;

    BaseCNN n1 cn1 n2 cn2 = {
      s1 = cn1.s ! n1.n ! Bare ;
      s2 = cn2.s ! n2.n ;
      n = Pl
      } ;
    DetCNN quant conj cnn = emptyNP ** {
      s = \\_ => quant.s ! cnn.n ! NotCmpd
              ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ! Bare ;
      a = agrP3 cnn.n
      } ;
    ReflPossCNN conj cnn = lin RNP (emptyNP ** {
      s = \\_ => cnn.s1 ++ conj.s2 ++ cnn.s2 ! Ezafe ++ "خود" ;
      a = agrP3 cnn.n ;
      animacy = Animate ;
      lock_NP = <>
      }) ;
    PossCNN_RNP quant conj cnn rnp = lin RNP (rnp ** {
      s = \\m => quant.s ! cnn.n ! NotCmpd
              ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ! Ezafe ++ rnp.s ! m ;
      a = agrP3 Pl ;
      lock_NP = <>
      }) ;

    FocusComp comp np = mkClause np (insertComp comp.s (predV beVerb)) ;

  oper
    finiteVP : Ant -> Pol -> Agr -> VPH -> Str = \ant,pol,agr,vp ->
      let tense = case ant.a of {
            Simul => Ind Pres Simul ;
            Anter => Ind Pres Anter
          }
      in showVPH (ta2vvt tense vp.vvtype) (VAor pol.p agr) agr vp ;

    complVVAnt : VV -> Ant -> Pol -> VPH -> Agr -> VVTense -> Str =
      \vv,ant,pol,vp,agr,tense ->
      if_then_Str vv.isAux conjThat [] ++
      case <ant.a,vv.compl> of {
        <Anter,_>     => showVPH PerfStem agr vp ++ subjAux pol.p agr ;
        <Simul,Indic> => showVPH (VAor pol.p agr) agr vp ;
        _             => showVPH (VSubj pol.p agr) agr vp
        } ;

    reciprocalNP : NP = emptyNP ** {
      s = \\_ => "یکدیگر" ;
      a = agrP3 Pl ;
      animacy = Animate
      } ;
}
