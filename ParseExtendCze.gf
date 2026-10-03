concrete ParseExtendCze of ParseExtend =
  ExtendCze - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron,
    theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash,
    A2VPSlash, CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP,
    ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN,
    NominalizeVPSlashNP, PiedPipingQuestSlash, PiedPipingRelSlash,
    ByVP, CompoundAP],
  NumeralCze - [num],
  PunctuationX
  ** open ResCze, Prelude, (G = GrammarCze) in {

lincat
  CNN = {n1,n2 : Determiner ; cn1,cn2 : Noun} ;

lin
  gen_Quant = {s = \\_,_,_ => []} ;
  num x = x ;

  ComplVV vv ant pol vp = {
    verb = vv ; clitPresent = andB vv.isAux vp.clitPresent ;
    clit = \\a => vv.refl ++ case vv.isAux of {True => vp.clit ! a ; False => []} ;
    compl = \\a => case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      case vv.isAux of {True => [] ; False => vp.clit ! a} ++ vp.compl ! a
    } ;

  EmbedVP ant pol pron vp = {
    s = case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      vp.clit ! pron.a ++ vp.compl ! pron.a
    } ;

  SlashV2V v ant pol vp = {
    verb = v ; clitPresent = False ; clit = \\_ => v.refl ;
    clitAfter = \\_ => [] ; c = v.c ; compl = \\_ => [] ;
    ind = \\a => case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      vp.clit ! a ++ vp.compl ! a
    } ;
  SlashVV vv ant pol vp = {
    verb = vv ; clitPresent = andB vv.isAux vp.clitPresent ;
    clit = \\a => vv.refl ++ case vv.isAux of {True => vp.clit ! a ; False => []} ;
    clitAfter = vp.clitAfter ; c = vp.c ;
    compl = \\a => case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      case vv.isAux of {True => [] ; False => vp.clit ! a} ++ vp.compl ! a ;
    ind = vp.ind
    } ;
  SlashV2VNP v np ant pol vp = {
    verb = v ; clitPresent = np.hasClit ;
    clit = \\a => v.refl ++ case hasCliticComplement v.c np.hasClit of {
      True => np.clit ! v.c.c ; False => []} ;
    clitAfter = vp.clitAfter ; c = vp.c ;
    compl = \\a => case hasCliticComplement v.c np.hasClit of {
      True => [] ; False => fullComplement v.c np.s np.prep} ++
      case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      vp.clit ! a ++ vp.compl ! a ; ind = vp.ind
    } ;

  InOrderToVP ant pol pron vp = {
    s = "aby" ++ case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      vp.clit ! pron.a ++ vp.compl ! pron.a
    } ;
  CompVP ant pol pron vp = {
    s = \\_ => case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      vp.clit ! pron.a ++ vp.compl ! pron.a
    } ;
  UttVP ant pol pron vp = {
    s = case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++
      vp.clit ! pron.a ++ vp.compl ! pron.a
    } ;

  ReflVPSlash vp rnp = {
    verb = vp.verb ; clitPresent = vp.clitPresent ;
    clit = \\a => vp.clit ! a ++ vp.clitAfter ! a ;
    compl = \\a => vp.compl ! a ++
      fullComplement vp.c (rnp.s ! a) (rnp.prep ! a) ++ vp.ind ! a
    } ;

  UttAP pron ap = {s = ap.pred ! pron.a} ;
  UttVPS pron vps = {s = vps.standalone ! pron.a} ;
  PhrUttMark pconj utt voc mark = {
    s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s
    } ;

  UseACard card = {s = \\_,_ => card.s ; size = Num5 ; head = CountedHead} ;
  UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s ; size = Num5 ; head = CountedHead} ;
  NumMore n = n ** {s = \\g,c => n.s ! g ! c ++ "více"} ;
  NumLess n = n ** {s = \\g,c => n.s ! g ! c ++ "méně"} ;
  BareN2 noun = noun ;
  TimeNP np = {s = np.s ! Acc} ;
  AdvAdv a b = {s = a.s ++ b.s} ;
  that_RP = G.IdRP ;
  whatSgFem_IP = {s = coForms ; a = Ag Fem Sg P3} ;
  whatSgNeut_IP = {s = coForms ; a = Ag Neutr Sg P3} ;
  AdnCAdv pol cadv = {s = pol.s ++ cadv.s} ;
  CAdvAP pol cadv ap comp = ap ** {
    s = \\g,n,c => pol.s ++ cadv.s ++ ap.s ! g ! n ! c ++ "než" ++ comp.s ! (Ag g n P3) ;
    pred = \\a => pol.s ++ cadv.s ++ ap.pred ! a ++ "než" ++ comp.s ! a ;
    isPost = True
    } ;
  ComparAdv pol cadv adv comp = {
    s = pol.s ++ cadv.s ++ adv.s ++ "než" ++ comp.s ! (Ag Neutr Sg P3)
    } ;
  ExtAdvAP ap adv = ap ** {
    s = \\g,n,c => ap.s ! g ! n ! c ++ SOFT_BIND ++ "," ++ adv.s ;
    pred = \\a => ap.pred ! a ++ SOFT_BIND ++ "," ++ adv.s ; isPost = True
    } ;
  EnoughAdv adv = {s = adv.s ++ "dostatečně"} ;
  EnoughAP ap ant pol vp = ap ** {
    s = \\g,n,c => ap.s ! g ! n ! c ++ "dost na to, aby" ++
      case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++ vp.compl ! (Ag g n P3) ;
    pred = \\a => ap.pred ! a ++ "dost na to, aby" ++
      case pol.p of {Pos => vp.verb.inf ; Neg => "ne" ++ BIND ++ vp.verb.inf} ++ vp.compl ! a ;
    isPost = True
    } ;
  FocusComp comp np = G.PredVP np (G.UseComp comp) ;
  ByVP vp = {s = "tím, že" ++ vp.verb.inf ++ vp.compl ! (Ag Neutr Sg P3)} ;
  CompoundAP noun adj =
    let ap = G.PositA adj in ap ** {
      s = \\g,n,c => noun.snom ++ ap.s ! g ! n ! c ;
      pred = \\a => noun.snom ++ ap.pred ! a ; isPost = False
      } ;
  RelNP np rs = np ** {
    s = \\c => np.s ! c ++ rs.s ! np.a ; prep = \\c => np.prep ! c ++ rs.s ! np.a ;
    clit = \\c => np.s ! c ++ rs.s ! np.a ; hasClit = False ; isDrop = False
    } ;
  ExtRelNP np rs = RelNP np rs ;
  RecipVPSlash vp = {
    verb = vp.verb ; clitPresent = vp.clitPresent ;
    clit = \\a => vp.clit ! a ++ vp.clitAfter ! a ;
    compl = \\a => vp.compl ! a ++ vp.c.s ++ "navzájem" ++ vp.ind ! a
    } ;
  RecipVPSlashCN vp cn = {
    verb = vp.verb ; clitPresent = vp.clitPresent ;
    clit = \\a => vp.clit ! a ++ vp.clitAfter ! a ;
    compl = \\a => vp.compl ! a ++ vp.c.s ++ cn.s ! Pl ! vp.c.c ++ "navzájem" ++ vp.ind ! a
    } ;

  BaseCNN n1 cn1 n2 cn2 = {
    n1 = n1 ; cn1 = cn1 ; n2 = n2 ; cn2 = cn2
    } ;
  DetCNN quant conj cnn = G.ConjNP conj (G.BaseNP
    (G.DetCN (G.DetQuant quant cnn.n1) cnn.cn1)
    (G.DetCN (G.DetQuant quant cnn.n2) cnn.cn2)) ;

  ReflPossCNN conj cnn = {
    s = \\a,c => reflCNNForm cnn.n1 cnn.cn1 c ++ conj.s2 ++ reflCNNForm cnn.n2 cnn.cn2 c ;
    prep = \\a,c => reflCNNForm cnn.n1 cnn.cn1 c ++ conj.s2 ++ reflCNNForm cnn.n2 cnn.cn2 c ;
    before = \\a,_ => [] ; prepBefore = \\a,_ => [] ; after = \\_ => [] ;
    m = AntecedentHead ; isPron = False
    } ;

  PossCNN_RNP quant conj cnn rnp = {
    s = \\a,c => quantCNNForm quant cnn.n1 cnn.cn1 c ++ conj.s2 ++
      quantCNNForm quant cnn.n2 cnn.cn2 c ++ rnp.s ! a ! Gen ;
    prep = \\a,c => quantCNNForm quant cnn.n1 cnn.cn1 c ++ conj.s2 ++
      quantCNNForm quant cnn.n2 cnn.cn2 c ++ rnp.prep ! a ! Gen ;
    before = \\_,_ => [] ; prepBefore = \\_,_ => [] ; after = \\_ => [] ;
    m = AntecedentHead ; isPron = False
    } ;

  PossPronRNP pron num cn rnp =
    let head = G.DetCN (G.DetQuant (G.PossPron pron) num) cn ;
        forms = appendNPForms head (rnp.s ! pron.a ! Gen)
    in head ** forms ** {clit = forms.s ; hasClit = False ; isDrop = False} ;

oper
  quantCNNForm : Quant -> Determiner -> Noun -> Case -> Str = \quant,num,cn,c ->
    quant.s ! nounGender cn (numSizeNumber num.size) ! numSizeNumber num.size ! c ++
    num.s ! nounGender cn (numSizeNumber num.size) ! c ++ numSizeForm cn.s num.size c ;

  reflCNNForm : Determiner -> Noun -> Case -> Str = \num,cn,c ->
    let quant = justDemPronFormsAdjective reflPossessivePron in
    quant.s ! nounGender cn (numSizeNumber num.size) ! numSizeNumber num.size ! c ++
    num.s ! nounGender cn (numSizeNumber num.size) ! c ++ numSizeForm cn.s num.size c ;

} ;
