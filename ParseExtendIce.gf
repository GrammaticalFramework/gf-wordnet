concrete ParseExtendIce of ParseExtend =
  CatIce, NumeralIce - [num], PunctuationX **
  open GrammarIce, Prelude, ResIce, (S = StructuralIce) in {

lincat
  CNN = {s1, s2 : Case => Str ; n1, n2 : Number ; g1, g2 : Gender} ;
  VPS = {s : Agr => Str} ;
  [VPS] = {s1, s2 : Agr => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1, s2 : Str} ;
  RNP = {s : NPCase => Str} ;
  RNPList = {s1, s2 : NPCase => Str} ;
  [Comp] = {s1, s2 : Agr => Str} ;
  [Imp] = {s1, s2 : Polarity => Number => Str} ;
  X = {s : Str} ;

lin
  gen_Quant = {
    s = \\_,_,_ => [] ;
    b = Suffix ;
    d = Strong ;
    isPron = False
  } ;
  UttAP pron ap = {s = ap.s ! pron.a.n ! pron.a.g ! Strong ! Nom} ;
  UttVPS pron vps = {s = vps.s ! pron.a} ;
  PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
  TPastSimple = {s = [] ; t = Past} ;

  FocusComp comp np =
    mkClause (comp.s ! np.a) (putDir (predV verbBe) (\\_ => np.s ! NCase Nom)) np.a ;

  BaseCNN num1 cn1 num2 cn2 = {
    s1 = \\c => num1.s ! cn1.g ! c ++ cn1.s ! num1.n ! Free ! Strong ! c ++ cn1.comp ! num1.n ! c ;
    s2 = \\c => num2.s ! cn2.g ! c ++ cn2.s ! num2.n ! Free ! Strong ! c ++ cn2.comp ! num2.n ! c ;
    n1 = num1.n ; n2 = num2.n ; g1 = cn1.g ; g2 = cn2.g
  } ;
  DetCNN quant conj cnn = {
    s = \\npc => let c = npcaseToCase npc in
      quant.s ! cnn.n1 ! cnn.g1 ! c ++ conj.s1 ++ cnn.s1 ! c ++ conj.s2 ++ cnn.s2 ! c ;
    a = gennumperToAgr (conjGender cnn.g1 cnn.g2) (conjNumber conj.n (conjNumber cnn.n1 cnn.n2)) P3 ;
    isPron = False
  } ;
  ReflPossCNN conj cnn = {s = \\npc => let c = npcaseToCase npc in
	conj.s1 ++ cnn.s1 ! c ++ reflPoss cnn.g1 cnn.n1 c ++
	conj.s2 ++ cnn.s2 ! c ++ reflPoss cnn.g2 cnn.n2 c
  } ;
  PossCNN_RNP quant conj cnn rnp = {s = \\npc => let c = npcaseToCase npc in
    quant.s ! cnn.n1 ! cnn.g1 ! c ++ conj.s1 ++ cnn.s1 ! c ++
    conj.s2 ++ cnn.s2 ! c ++ rnp.s ! NCase Gen
  } ;

  NumMore num = num ** {s = \\g,c => num.s ! g ! c ++ "til viðbótar" ; n = Pl} ;
  NumLess num = num ** {s = \\g,c => num.s ! g ! c ++ "færri" ; n = Pl} ;
  UseACard card = {s = \\_,_ => card.s ; n = Pl} ;
  UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s ; n = Pl} ;

  RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.a} ;
  ExtRelNP np rs = np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.a} ;
  BareN2 n2 = n2 ;
  ComparAdv pol cadv adv comp = {s = pol.s ++ negation pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! gennumperToAgr Neutr Sg P3} ;
  CAdvAP pol cadv ap comp = {s = \\n,g,d,c => pol.s ++ negation pol.p ++ cadv.s ++
    ap.s ! n ! g ! d ! c ++ cadv.p ++ comp.s ! gennumperToAgr g n P3
  } ;
  AdnCAdv pol cadv = {s = pol.s ++ negation pol.p ++ cadv.s ++ cadv.p} ;
  EnoughAP ap ant pol vp = {s = \\n,g,d,c => ap.s ! n ! g ! d ! c ++
    "nógu til að" ++ ant.s ++ pol.s ++ infPolVP vp pol.p
  } ;
  EnoughAdv adv = {s = adv.s ++ "nógu"} ;
  ExtAdvAP ap adv = {s = \\n,g,d,c => ap.s ! n ! g ! d ! c ++ "," ++ adv.s} ;
  TimeNP np = {s = np.s ! NCase Acc} ;
  AdvAdv first second = {s = first.s ++ second.s} ;

  whatSgFem_IP = {s = \\_,c => S.whoSg_IP.s ! Fem ! c ; n = Sg} ;
  whatSgNeut_IP = {s = \\_,c => S.whatSg_IP.s ! Neutr ! c ; n = Sg} ;
  that_RP = IdRP ;

  EmbedVP ant pol pron vp = {s = "að" ++ ant.s ++ pol.s ++ infPolVPAgr vp pol.p pron.a} ;
  ComplVV vv ant pol vp = putDir (predV vv) (\\a => vv.c2.s ++ ant.s ++ pol.s ++ infPolVPAgr vp pol.p a) ;
  SlashVV vv ant pol slash =
    (putDir (predV vv) (\\a => vv.c2.s ++ ant.s ++ pol.s ++ infPolVPAgr slash pol.p a)) ** {c2 = slash.c2} ;
  SlashV2V verb ant pol vp =
    (putDir (predV verb) (\\a => verb.c3.s ++ ant.s ++ pol.s ++ infPolVPAgr vp pol.p a)) ** {c2 = verb.c2} ;
  SlashV2VNP verb np ant pol slash =
    (putDir (predV verb) (\\_ => verb.c3.s ++ np.s ! NCase verb.c3.c ++ ant.s ++ pol.s ++ infPolVPAgr slash pol.p np.a)) ** {c2 = verb.c2} ;
  InOrderToVP ant pol pron vp = {s = "til þess að" ++ ant.s ++ pol.s ++ infPolVPAgr vp pol.p pron.a} ;
  CompVP ant pol pron vp = {s = \\_ => "að" ++ ant.s ++ pol.s ++ infPolVPAgr vp pol.p pron.a} ;
  UttVP ant pol pron vp = {s = "að" ++ ant.s ++ pol.s ++ infPolVPAgr vp pol.p pron.a} ;

  ReflA2 adjective rnp = {s = \\n,g,d,c => adjective.s ! APosit d n g c ++ adjective.c2.s ++ rnp.s ! NCase adjective.c2.c} ;
  ReflVPSlash slash rnp = putDir slash (\\_ => slash.c2.s ++ rnp.s ! NCase slash.c2.c) ;
  RecipVPSlash slash = putDir slash (\\_ => slash.c2.s ++ "hvor annan") ;
  RecipVPSlashCN slash cn = putDir slash (\\_ => slash.c2.s ++ cn.s ! Sg ! Suffix ! Strong ! Acc ++ "hvers annars") ;

  GenModNP num np cn = {
    s = \\npc => let c = npcaseToCase npc in
	  cn.s ! num.n ! Free ! Strong ! c ++ cn.comp ! num.n ! c ++ np.s ! NCase Gen ;
    a = gennumperToAgr cn.g num.n P3 ; isPron = False
  } ;
  EmptyRelSlash slash = {s = \\tense,ant,pol,_ => slash.s ! tense ! ant ! pol ! ODir ++ slash.c2.s} ;

  MkVPS temp pol vp = {s = \\agr => temp.s ++ pol.s ++ infVPPlus vp (VPMood temp.t temp.a) pol.p agr} ;
  BaseVPS first second = {s1 = first.s ; s2 = second.s} ;
  ConsVPS first rest = {s1 = \\a => first.s ! a ++ "," ++ rest.s1 ! a ; s2 = rest.s2} ;
  ConjVPS conj list = {s = \\a => conj.s1 ++ list.s1 ! a ++ conj.s2 ++ list.s2 ! a} ;
  PredVPS np vps = {s = np.s ! NCase Nom ++ vps.s ! np.a} ;

  MkVPI vp = {s = infVP vp} ;
  BaseVPI first second = {s1 = first.s ; s2 = second.s} ;
  ConsVPI first rest = {s1 = first.s ++ "," ++ rest.s1 ; s2 = rest.s2} ;
  ConjVPI conj list = {s = conj.s1 ++ list.s1 ++ conj.s2 ++ list.s2} ;
  ComplVPIVV vv vpi = putDir (predV vv) (\\_ => vv.c2.s ++ vpi.s) ;

  BaseImp first second = {s1 = first.s ; s2 = second.s} ;
  ConsImp first rest = {s1 = \\p,n => first.s ! p ! n ++ "," ++ rest.s1 ! p ! n ; s2 = rest.s2} ;
  ConjImp conj list = {s = \\p,n => conj.s1 ++ list.s1 ! p ! n ++ conj.s2 ++ list.s2 ! p ! n} ;
  BaseComp first second = {s1 = first.s ; s2 = second.s} ;
  ConsComp first rest = {s1 = \\a => first.s ! a ++ "," ++ rest.s1 ! a ; s2 = rest.s2} ;
  ConjComp conj list = {s = \\a => conj.s1 ++ list.s1 ! a ++ conj.s2 ++ list.s2 ! a} ;

  PresPartAP vp = {s = \\n,g,_,_ => vp.p ! PPres ++ vpObjects vp (gennumperToAgr g n P3)} ;
  PastPartAP slash = {s = \\n,g,_,c => slash.p ! PStrong n g c ++ vpObjects slash (gennumperToAgr g n P3)} ;
  PastPartAgentAP slash np = {s = \\n,g,_,c => slash.p ! PStrong n g c ++
    vpObjects slash (gennumperToAgr g n P3) ++ "af" ++ np.s ! NCase Dat
  } ;
  PassVPSlash slash = passiveVP slash [] ;
  PassAgentVPSlash slash np = passiveVP slash ("af" ++ np.s ! NCase Dat) ;
  ProgrVPSlash slash = slash ;
  ComplBareVS verb sentence = putDir (predV verb) (\\_ => sentence.s) ;
  ComplSlashPartLast slash np = putDir slash (\\a => slash.c2.s ++ np.s ! NCase slash.c2.c ++ slash.dirObj ! a) ;

  ReflPron = {s = \\npc => case npcaseToCase npc of {Nom => "sjálfur" ; Acc => "sig" ; Dat => "sér" ; Gen => "sín"}} ;
  ReflPoss num cn = {s = \\npc => let c = npcaseToCase npc in
    cn.s ! num.n ! Suffix ! Strong ! c ++ cn.comp ! num.n ! c ++ reflPoss cn.g num.n c
  } ;
  AdvRNP np prep rnp = {s = \\c => np.s ! c ++ prep.s ++ rnp.s ! NCase prep.c} ;
  AdvRVP vp prep rnp = vp ** {a2 = vp.a2 ++ prep.s ++ rnp.s ! NCase prep.c} ;
  AdvRAP ap prep rnp = {s = \\n,g,d,c => ap.s ! n ! g ! d ! c ++ prep.s ++ rnp.s ! NCase prep.c} ;
  PossPronRNP pron num cn rnp = {
    s = \\npc => let c = npcaseToCase npc in cn.s ! num.n ! Free ! Strong ! c ++
      pron.s ! NCase Gen ++ cn.comp ! num.n ! c ++ rnp.s ! NCase Gen ;
    a = gennumperToAgr cn.g num.n P3 ; isPron = False
  } ;

  CompoundN modifier head = {s = \\n,sp,c => modifier.s ! Sg ! Free ! Gen ++ BIND ++ head.s ! n ! sp ! c ; g = head.g} ;
  CompoundAP noun adjective = {s = \\n,g,d,c => noun.s ! Sg ! Free ! Gen ++ BIND ++ adjective.s ! APosit d n g c} ;
  GerundCN vp = {s = \\_,_,_,_ => "að" ++ infVP vp ; comp = \\_,_ => [] ; g = Neutr} ;
  GerundNP vp = {s = \\_ => "að" ++ infVP vp ; a = gennumperToAgr Neutr Sg P3 ; isPron = False} ;
  GerundAdv vp = {s = "með því að" ++ infVP vp} ;
  ByVP vp = {s = "með því að" ++ infVP vp} ;
  ApposNP np app = np ** {s = \\c => np.s ! c ++ "," ++ app.s ! NCase Nom} ;
  PositAdVAdj adjective = {s = adjective.adv} ;
  UttVPShort vp = {s = infVP vp} ;

  UseDAP dap = dapNP Neutr dap ;
  UseDAPMasc dap = dapNP Masc dap ;
  UseDAPFem dap = dapNP Fem dap ;
  num x = {s = \\ngc => x.s ! ngc ; n = sizeToNumber x.size} ;

oper
  putDir : ResIce.VP -> (Agr => Str) -> ResIce.VP = \vp,obj -> vp ** {dirObj = obj} ;
  infPolVP : ResIce.VP -> Polarity -> Str = \vp,pol -> infVPPlus vp VPInf pol (gennumperToAgr Neutr Sg P3) ;
  infPolVPAgr : ResIce.VP -> Polarity -> Agr -> Str = \vp,pol,agr -> infVPPlus vp VPInf pol agr ;
  vpObjects : ResIce.VP -> Agr -> Str = \vp,a -> vp.indObj ! a ++ vp.dirObj ! a ++ vp.a2 ;

  passiveVP : VPSlash -> Str -> ResIce.VP = \slash,agent -> let vp = predV verbBe in {
    s = \\form,pol,agr => let be = vp.s ! form ! pol ! agr in
      vf be.fin (slash.p ! PStrong agr.n agr.g Nom) (be.a1.p1 ++ be.a1.p2) True ;
    p = \\pform => slash.p ! pform ;
    indObj = slash.indObj ;
    dirObj = \\a => slash.dirObj ! a ++ agent ;
    a2 = slash.a2 ;
    indShift = slash.indShift ;
    dirShift = False
  } ;

  reflPoss : Gender -> Number -> Case -> Str = \g,n,c -> case <g,n> of {
    <Masc,Sg> => caseList "sinn" "sinn" "sínum" "síns" ! c ;
    <Fem,Sg> => caseList "sín" "sína" "sinni" "sinnar" ! c ;
    <Neutr,Sg> => caseList "sitt" "sitt" "sínu" "síns" ! c ;
    <Masc,Pl> => caseList "sínir" "sína" "sínum" "sinna" ! c ;
    <Fem,Pl> => caseList "sínar" "sínar" "sínum" "sinna" ! c ;
    <Neutr,Pl> => caseList "sín" "sín" "sínum" "sinna" ! c
  } ;
  dapNP : Gender -> DAP -> ResIce.NP = \g,dap -> {
    s = \\c => dap.s ! g ! npcaseToCase c ;
    a = gennumperToAgr g dap.n P3 ;
    isPron = False
  } ;
}
