concrete ParseExtendJpn of ParseExtend =
  ExtraJpn - [Level,Part,Honorific,Informal,PartWA,PartGA,StylePartPhr,
              Foc,FocObj,FocAdv,FocAdV,FocAP,FocNeg,FocVP,FocVV,UseFoc,
              GenNP,N2VPSlash,A2VPSlash,PurposeVP,ComplGenVV,ReflRNP,
              ReflA2RNP,AdvIsNPAP,ExistCN,NominalizeVPSlashNP,
              PiedPipingQuestSlash,PiedPipingRelSlash],
  NumeralJpn - [num], PunctuationX **
  open Prelude, ResJpn, ParadigmsJpn, (G=GrammarJpn) in {

lincat
  CNN = {num1,num2 : ResJpn.Num ; cn1,cn2 : CN} ;
  RNP = ResJpn.NP ;
  [Comp] = {s : Style => Str} ;
  [Imp] = {s : Style => Polarity => Str} ;

lin
  gen_Quant = G.DefArt ;

  UttAP pron ap = {
    s = \\part,st => ap.prepositive ! st ++
                    ap.pred ! st ! TPres ! Pos ;
    type = NoImp
    } ;

  UttVPS pron vps = {
    s = \\part,st => vps.prepositive ! st ++
                    vps.obj ! st ++ vps.prep ++
                    vps.verb ! case pron.Pron1Sg of {True => Me ; False => SomeoneElse}
                             ! pron.anim ! st ;
    type = NoImp
    } ;

  PhrUttMark pconj utt voc mark = {
    s = CAPIT ++ pconj.s ++ utt.s ! Wa ! Resp ++ voc.s ! Resp ++
        SOFT_BIND ++ mark.s
    } ;

  FocusComp comp np = G.PredVP np (G.UseComp comp) ;

  BaseComp first second = {
    s = \\st => compText first st ++ "と" ++ compText second st
    } ;
  ConsComp first rest = {
    s = \\st => compText first st ++ "、" ++ rest.s ! st
    } ;
  ConjComp conj comps = {
    verb = \\a => mkCopula.s ;
    te = \\a,st => mkCopula.te ; ba = \\a,st => mkCopula.ba ;
    a_stem = \\a,st => mkCopula.a_stem ; i_stem = \\a,st => mkCopula.i_stem ;
    obj = comps.s ; comparison = comps.s ;
    prepositive = \\st => [] ; needSubject = True
    } ;

  BaseImp first second = {
    s = \\st,p => first.s ! st ! p ++ "、" ++ second.s ! st ! p
    } ;
  ConsImp first rest = {
    s = \\st,p => first.s ! st ! p ++ "、" ++ rest.s ! st ! p
    } ;
  ConjImp conj imps = imps ;

  BaseCNN num1 cn1 num2 cn2 = {
    num1 = num1 ; cn1 = cn1 ; num2 = num2 ; cn2 = cn2
    } ;

  DetCNN quant conj cnn = G.ConjNP conj (G.BaseNP
    (G.DetCN (G.DetQuant quant cnn.num1) cnn.cn1)
    (G.DetCN (G.DetQuant quant cnn.num2) cnn.cn2)) ;

  ReflPossCNN conj cnn = G.ConjNP conj (G.BaseNP
    (reflPossNP cnn.num1 cnn.cn1) (reflPossNP cnn.num2 cnn.cn2)) ;

  PossCNN_RNP quant conj cnn rnp = G.ConjNP conj (G.BaseNP
    (G.DetCN (G.DetQuant quant cnn.num1) (G.PossNP cnn.cn1 rnp))
    (G.DetCN (G.DetQuant quant cnn.num2) (G.PossNP cnn.cn2 rnp))) ;

  NumMore n = n ** {s = n.s ++ "余分の"} ;
  NumLess n = n ** {s = n.s ++ "少ない"} ;

  UseACard card = {s = card.s ; postpositive = [] ; n = Pl ; numType = NoCard} ;
  UseAdAACard ada card = {s = ada.s ++ card.s ; postpositive = [] ; n = Pl ; numType = NoCard} ;

  RelNP np rs = np ** {s = \\st => rs.s ! np.anim ! st ++ np.s ! st} ;
  ExtRelNP = G.RelNP ;

  ExtAdvAP ap adv = ap ** {
    prepositive = \\st => ap.prepositive ! st ++ adv.s ! st
    } ;

  BareN2 n = lin N n ;

  ComparAdv pol cadv adv comp = {
    s = \\st => comparisonText comp st ++ cadv.s ++ adv.s ! st ;
    prepositive = False
    } ;

  CAdvAP pol cadv ap comp = ap ** {
    pred = \\st,t,p => comparisonText comp st ++ cadv.s ++ ap.pred ! st ! t ! p ;
    attr = \\st => comparisonText comp st ++ cadv.s ++ ap.attr ! st ;
    te = \\st,p => comparisonText comp st ++ cadv.s ++ ap.te ! st ! p ;
    ba = \\st,p => comparisonText comp st ++ cadv.s ++ ap.ba ! st ! p ;
    adv = \\st => comparisonText comp st ++ cadv.s ++ ap.adv ! st ;
    dropNaEnging = \\st => comparisonText comp st ++ cadv.s ++ ap.dropNaEnging ! st
    } ;

  AdnCAdv pol cadv = {s = cadv.s_adn ; postposition = True} ;

  EnoughAP ap ant pol vp = ap ** {
    pred = \\st,t,p => vpInf vp st pol.b ++ "ほど" ++ ap.pred ! st ! t ! p ;
    attr = \\st => vpInf vp st pol.b ++ "ほど" ++ ap.attr ! st ;
    prepositive = \\st => ap.prepositive ! st ++ vp.prepositive ! st
    } ;

  EnoughAdv adv = adv ** {s = \\st => "十分" ++ adv.s ! st} ;

  TimeNP np = {s = \\st => np.prepositive ! st ++ np.s ! st ++ "に" ; prepositive = True} ;
  AdvAdv adv1 adv2 = adv2 ** {s = \\st => adv1.s ! st ++ adv2.s ! st} ;

  whatSgFem_IP, whatSgNeut_IP = G.whatSg_IP ;
  that_RP = G.IdRP ;

  EmbedVP ant pol pron vp = {
    s = \\part,st => vpInf vp st pol.b ; isVP = True
    } ;

  ComplVV vv ant pol vp = G.ComplVV vv (setVPPol ant pol vp) ;
  SlashVV vv ant pol slash = G.SlashVV vv (setSlashPol ant pol slash) ;
  SlashV2V v ant pol vp = G.SlashV2V v (setVPPol ant pol vp) ;
  SlashV2VNP v np ant pol slash = G.SlashV2VNP v np (setSlashPol ant pol slash) ;

  InOrderToVP ant pol pron vp = {
    s = \\st => vpInf vp st pol.b ++ "ために" ; prepositive = True
    } ;

  CompVP ant pol pron vp = {
    verb = \\_ => mkCopula.s ;
    te = \\_,st => mkCopula.te ; ba = \\_,st => mkCopula.ba ;
    a_stem = \\_,st => mkCopula.a_stem ; i_stem = \\_,st => mkCopula.i_stem ;
    obj = \\st => vpInf vp st pol.b ++ "こと" ;
    comparison = \\st => vpInf vp st pol.b ++ "こと" ;
    prepositive = vp.prepositive ; needSubject = True
    } ;

  UttVP ant pol pron vp = {
    s = \\part,st => pron.s ! st ++ "は" ++ vp.prepositive ! st ++
                    vp.obj ! st ++ vp.prep ++ finiteVP ant pol pron vp st ;
    type = NoImp
    } ;

  ReflA2 a rnp = G.ComplA2 a rnp ;
  ReflVPSlash slash rnp = G.ComplSlash slash rnp ;
  RecipVPSlash slash = G.ComplSlash slash reciprocalNP ;
  RecipVPSlashCN slash cn = G.ComplSlash slash
    (G.MassNP (G.PossNP cn reciprocalNP)) ;

  ReflPron = selfNP ;
  ReflPoss num cn = reflPossNP num cn ;
  AdvRNP np prep rnp = rnp ** {
    s = \\st => np.prepositive ! st ++ np.s ! st ++ prep.s ++ rnp.s ! st
    } ;
  AdvRVP vp prep rnp = vp ** {
    obj = \\st => vp.obj ! st ++ rnp.prepositive ! st ++ rnp.s ! st ++ prep.s
    } ;
  AdvRAP ap prep rnp = ap ** {
    pred = \\st,t,p => rnp.s ! st ++ prep.s ++ ap.pred ! st ! t ! p ;
    attr = \\st => rnp.s ! st ++ prep.s ++ ap.attr ! st ;
    prepositive = \\st => rnp.prepositive ! st ++ ap.prepositive ! st
    } ;
  PossPronRNP pron num cn rnp = G.DetCN (G.DetQuant (G.PossPron pron) num)
    (G.PossNP cn rnp) ;

  GenModNP num np cn = G.DetCN (G.DetQuant G.DefArt num) (G.PossNP cn np) ;
  CompoundN first head = head ** {
    s = \\n,st => first.s ! Sg ! st ++ head.s ! n ! st
    } ;

  PresPartAP vp = progressiveAP vp ;
  PastPartAP slash = passiveAP (\\st => []) slash ;
  PastPartAgentAP slash agent = passiveAP (\\st =>
    agent.prepositive ! st ++ agent.s ! st ++ "によって") slash ;

  PassVPSlash slash = slashVP slash ;
  PassAgentVPSlash slash agent = (slashVP slash) ** {
    obj = \\st => agent.prepositive ! st ++ agent.s ! st ++ "によって" ++ slash.obj ! st
    } ;
  ProgrVPSlash slash = slash ** {
    s = \\sp,st,t,p => slash.te ! sp ! Pos ++ (mkVerb "いる" Gr2).s ! st ! t ! p
    } ;

  CompoundAP noun adj = verbalAP (\\st =>
    noun.s ! Sg ! st ++ adj.attr) ;

  GerundCN vp = gerundCN vp ;
  GerundNP vp = G.MassNP (gerundCN vp) ;
  GerundAdv vp = {s = \\st => vpInf vp st Pos ++ "ことで" ; prepositive = True} ;
  ByVP vp = {s = \\st => vpInf vp st Pos ++ "ことによって" ; prepositive = True} ;

  ApposNP first second = first ** {
    s = \\st => first.s ! st ++ "、" ++ second.s ! st ++ "、"
    } ;
  PositAdVAdj adj = {s = adj.adv ! Pos} ;

  ComplBareVS = G.ComplVS ;
  ComplSlashPartLast = G.ComplSlash ;
  UttVPShort = G.UttVP ;

  UseDAP, UseDAPMasc, UseDAPFem = \det -> {
    s = det.sp ; prepositive = \\st => [] ; needPart = True ;
    changePolar = det.no ; meaning = SomeoneElse ; anim = Inanim
    } ;

  TPastSimple = G.TPast ;
  num value = value ;

oper
  selfNP : ResJpn.NP = {
    s = \\st => "自分" ; prepositive = \\st => [] ; needPart = True ;
    changePolar = False ; meaning = SomeoneElse ; anim = Anim
    } ;

  reciprocalNP : ResJpn.NP = selfNP ** {s = \\st => "お互い"} ;

  reflPossNP : ResJpn.Num -> CN -> ResJpn.NP = \num,cn -> {
    s = \\st => "自分の" ++ cn.object ! st ++ cn.s ! num.n ! st ;
    prepositive = cn.prepositive ; needPart = True ; changePolar = False ;
    meaning = SomeoneElse ; anim = cn.anim
    } ;

  compText : Comp -> Style -> Str = \comp,st ->
    comp.prepositive ! st ++ comp.obj ! st ++
    comp.verb ! Inanim ! st ! TPres ! Pos ;

  comparisonText : Comp -> Style -> Str = \comp,st ->
    comp.prepositive ! st ++ comp.comparison ! st ;

  vpInf : ResJpn.VP -> Style -> Polarity -> Str = \vp,st,p ->
    vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
    vp.verb ! SomeoneElse ! Inanim ! Plain ! TPres ! p ;

  finiteVP : Ant -> Pol -> Pron -> ResJpn.VP -> Style -> Str = \ant,pol,pron,vp,st ->
    case ant.a of {
      Simul => vp.verb ! case pron.Pron1Sg of {True => Me ; False => SomeoneElse}
                       ! pron.anim ! st ! TPres ! pol.b ;
      Anter => vp.verb ! case pron.Pron1Sg of {True => Me ; False => SomeoneElse}
                       ! pron.anim ! st ! TPast ! pol.b
      } ;

  setVPPol : Ant -> Pol -> ResJpn.VP -> ResJpn.VP = \ant,pol,vp -> vp ** {
    verb = \\sp,a,st,t,p => case ant.a of {
      Simul => vp.verb ! sp ! a ! st ! TPres ! pol.b ;
      Anter => vp.verb ! sp ! a ! st ! TPast ! pol.b
      }
    } ;

  setSlashPol : Ant -> Pol -> VPSlash -> VPSlash = \ant,pol,vp -> vp ** {
    s = \\sp,st,t,p => case ant.a of {
      Simul => vp.s ! sp ! st ! TPres ! pol.b ;
      Anter => vp.s ! sp ! st ! TPast ! pol.b
      }
    } ;

  slashInf : VPSlash -> Style -> TTense -> Polarity -> Str = \slash,st,t,p ->
    slash.prepositive ! st ++ slash.obj ! st ++ slash.s ! SomeoneElse ! Plain ! t ! p ;

  slashVP : VPSlash -> ResJpn.VP = \slash -> {
    verb = \\sp,a,st,t,p => slash.pass ! sp ! st ! t ! p ;
    a_stem = \\sp,a,st => slash.a_stem ! sp ;
    i_stem = \\sp,a,st => slash.i_stem ! sp ;
    te = \\sp,a,st,p => slash.pass_te ! sp ! p ;
    ba = \\sp,a,st,p => slash.ba ! sp ! p ;
    prep = [] ; obj = slash.obj ; prepositive = slash.prepositive ; needSubject = True
    } ;

  slashPassInf : VPSlash -> Style -> TTense -> Polarity -> Str = \slash,st,t,p ->
    slash.prepositive ! st ++ slash.obj ! st ++
    slash.pass ! SomeoneElse ! Plain ! t ! p ;

  verbalAP : (Style => Str) -> AP = \form -> lin AP {
    pred = \\st,t,p => form ! st ++ mkCopula.s ! st ! t ! p ;
    attr = form ; te = \\st,p => form ! st ++ mkCopula.te ! p ;
    ba = \\st,p => form ! st ++ mkCopula.ba ! p ;
    adv = \\st => form ! st ++ "ように" ; dropNaEnging = form ;
    prepositive = \\st => [] ; needSubject = True
    } ;

  progressiveAP : ResJpn.VP -> AP = \vp ->
    let iru = mkVerb "いる" Gr2 in lin AP {
      pred = \\st,t,p => vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
                         vp.te ! SomeoneElse ! Inanim ! st ! Pos ++ iru.s ! st ! t ! p ;
      attr = \\st => vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
                     vp.te ! SomeoneElse ! Inanim ! st ! Pos ++ "いる" ;
      te = \\st,p => vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
                     vp.te ! SomeoneElse ! Inanim ! st ! Pos ++ iru.te ! p ;
      ba = \\st,p => vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
                     vp.te ! SomeoneElse ! Inanim ! st ! Pos ++ iru.ba ! p ;
      adv = \\st => vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
                    vp.te ! SomeoneElse ! Inanim ! st ! Pos ++ iru.te ! Pos ;
      dropNaEnging = \\st => vp.prepositive ! st ++ vp.obj ! st ++ vp.prep ++
                             vp.te ! SomeoneElse ! Inanim ! st ! Pos ++ "いる" ;
      prepositive = \\st => [] ; needSubject = True
      } ;

  passiveAP : (Style => Str) -> VPSlash -> AP = \agent,slash -> lin AP {
    pred = \\st,t,p => agent ! st ++ slash.prepositive ! st ++ slash.obj ! st ++
                       slash.pass ! SomeoneElse ! st ! TPast ! p ;
    attr = \\st => agent ! st ++ slash.prepositive ! st ++ slash.obj ! st ++
                   slash.pass ! SomeoneElse ! Plain ! TPast ! Pos ;
    te = \\st,p => agent ! st ++ slash.prepositive ! st ++ slash.obj ! st ++
                   slash.pass_te ! SomeoneElse ! p ;
    ba = \\st,p => agent ! st ++ slash.prepositive ! st ++ slash.obj ! st ++
                   slash.ba ! SomeoneElse ! p ;
    adv = \\st => agent ! st ++ slash.prepositive ! st ++ slash.obj ! st ++
                  slash.pass_te ! SomeoneElse ! Pos ;
    dropNaEnging = \\st => agent ! st ++ slash.prepositive ! st ++ slash.obj ! st ++
                           slash.pass ! SomeoneElse ! Plain ! TPast ! Pos ;
    prepositive = \\st => [] ; needSubject = True
    } ;

  gerundCN : ResJpn.VP -> CN = \vp -> lin CN (regNoun (vpInf vp Plain Pos ++ "こと")
    Inanim "つ" False True) ** {
      object = \\st => [] ; prepositive = \\st => [] ; hasAttr = False
    } ;
}
