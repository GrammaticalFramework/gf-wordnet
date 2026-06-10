--# -path=.:../gf-rgl/src/ukrainian:../gf-rgl/src/abstract:../gf-rgl/src/common:prelude
concrete ParseExtendUkr of ParseExtend =
  CatUkr,
  NumeralUkr - [num], PunctuationX **
  open ResUkr, (R = ParamX), Prelude in {

lincat
  CNN = {s : Case => Number => Str; g : Gender; n : Number} ;
  VPS = {s : Gender => Number => Person => Str} ;
  [VPS] = {s1,s2 : Gender => Number => Person => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  VPS2 = {s : Gender => Number => Person => Str; c : Compl; post : Str} ;
  [VPS2] = {s1,s2 : Gender => Number => Person => Str; c : Compl; post : Str} ;
  VPI2 = {s : Str; c : Compl; post : Str} ;
  [VPI2] = {s1,s2 : Str; c : Compl; post : Str} ;
  [Comp] = {s1,s2 : Gender => Number => Str} ;
  [Imp] = {s1,s2 : R.Polarity => Number => Str} ;
  RNP = {s : Case => Str} ;
  RNPList = {s1,s2 : Case => Str} ;
  X = {s : Str} ;

lin
  gen_Quant = {s = \\_,_,_ => []} ;

  UttAP p ap = {s = ap.s ! Nom ! genNum p.g p.n} ;
  UttVPS p vps = {s = vps.s ! p.g ! p.n ! p.p} ;
  PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
  GenModNP num np cn = {
    s = \\c => np.s ! Gen ++ cn.s ! c ! num.n ;
    g = cn.g ;
    n = num.n ;
    p = P3
  } ;
  EmptyRelSlash cls = {s = \\_,_ => cls.s ! R.Pres ! R.Pos} ;

  ReflA2 a rnp = a ** {
    s = \\c,gn => a.s ! c ! gn ++ prepNP a.c2 rnp
  } ;
  ReflVPSlash slash rnp = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ prepNP slash.c rnp ++ slash.post ;
    inf = slash.inf ++ prepNP slash.c rnp ++ slash.post ;
    imp = \\pol,n => slash.imp ! pol ! n ++ prepNP slash.c rnp ++ slash.post
  } ;

  BaseCNN num1 cn1 num2 cn2 = {
    s = \\c,n => cn1.s ! c ! num1.n ++ cn2.s ! c ! num2.n ;
    g = cn2.g ;
    n = Pl
  } ;
  DetCNN quant conj cnn = {
    s = \\c => quant.s ! c ! cnn.g ! cnn.n ++ cnn.s ! c ! cnn.n ;
    g = cnn.g ;
    n = cnn.n ;
    p = P3
  } ;
  ReflPossCNN conj cnn = {
    s = \\c => "свій" ++ cnn.s ! c ! cnn.n
  } ;
  PossCNN_RNP quant conj cnn rnp = {
    s = \\c => quant.s ! c ! cnn.g ! cnn.n ++ cnn.s ! c ! cnn.n ++ rnp.s ! Gen
  } ;

  NumLess num = num ** {s = num.s ++ "менше"} ;
  NumMore num = num ** {s = num.s ++ "більше"} ;
  num n = {s = n.s} ;
  UseACard card = {s = card.s; n = card.n} ;
  UseAdAACard ada card = {s = ada.s ++ card.s; n = card.n} ;

  RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.g ! np.n} ;
  ExtRelNP np rs = np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.g ! np.n} ;
  ExtAdvAP ap adv = ap ** {s = \\c,gn => ap.s ! c ! gn ++ "," ++ adv.s} ;
  BareN2 n = n ;

  ComparAdv pol cadv adv comp = {
    s = neg pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! Masc ! Sg
  } ;
  CAdvAP pol cadv ap comp = {
    s = \\c,gn => neg pol.p ++ cadv.s ++ ap.s ! c ! gn ++ cadv.p ++ comp.s ! Masc ! Sg
  } ;
  AdnCAdv pol cadv = {s = neg pol.p ++ cadv.s ++ cadv.p} ;
  EnoughAP ap ant pol vp = ap ** {
    s = \\c,gn => ap.s ! c ! gn ++ "достатньо" ++ neg pol.p ++ vp.inf
  } ;
  EnoughAdv adv = {s = adv.s ++ "достатньо"} ;
  TimeNP np = {s = np.s ! Acc} ;
  AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

  whatSgFem_IP = {s=\\_=>"що"; g=Fem; n=Sg; p=P3} ;
  whatSgNeut_IP = {s=\\_=>"що"; g=Neuter; n=Sg; p=P3} ;
  that_RP = {s = \\_,_,_ => "що"} ;

  EmbedVP ant pol pron vp = {s = ant.s ++ pol.s ++ neg pol.p ++ vp.inf} ;
  ComplVV vv ant pol vp = {
    s = \\t,p,g,n,pe => finiteVerb vv t p g n pe ++ ant.s ++ pol.s ++ neg pol.p ++ vp.inf ;
    inf = vv.infinitive ++ ant.s ++ pol.s ++ neg pol.p ++ vp.inf ;
    imp = \\p,n => neg p ++ vv.imperative2 ! n ++ ant.s ++ pol.s ++ neg pol.p ++ vp.inf
  } ;
  SlashVV vv ant pol slash = slash ** {
    s = \\t,p,g,n,pe => finiteVerb vv t p g n pe ++ ant.s ++ pol.s ++ neg pol.p ++ slash.inf ;
    inf = vv.infinitive ++ ant.s ++ pol.s ++ neg pol.p ++ slash.inf ;
    imp = \\p,n => neg p ++ vv.imperative2 ! n ++ ant.s ++ pol.s ++ neg pol.p ++ slash.inf
  } ;
  SlashV2V v ant pol vp = {
    s = \\t,p,g,n,pe => finiteVerb v t p g n pe ;
    inf = v.infinitive ;
    imp = \\p,n => neg p ++ v.imperative2 ! n ;
    c = v.c2 ;
    post = ant.s ++ pol.s ++ neg pol.p ++ vp.inf
  } ;
  SlashV2VNP v np ant pol slash = slash ** {
    s = \\t,p,g,n,pe => finiteVerb v t p g n pe ++ prepNP v.c2 np ++ ant.s ++ pol.s ++ neg pol.p ++ slash.inf ;
    inf = v.infinitive ++ prepNP v.c2 np ++ ant.s ++ pol.s ++ neg pol.p ++ slash.inf ;
    imp = \\p,n => neg p ++ v.imperative2 ! n ++ prepNP v.c2 np ++ ant.s ++ pol.s ++ neg pol.p ++ slash.inf
  } ;
  InOrderToVP ant pol pron vp = {s = "щоб" ++ neg pol.p ++ vp.inf} ;
  CompVP ant pol pron vp = {s = \\_,_ => neg pol.p ++ vp.inf} ;
  UttVP ant pol pron vp = {s = neg pol.p ++ vp.inf} ;
  RecipVPSlash slash = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ "один одного" ++ slash.post ;
    inf = slash.inf ++ "один одного" ++ slash.post ;
    imp = \\pol,n => slash.imp ! pol ! n ++ "один одного" ++ slash.post
  } ;
  RecipVPSlashCN slash cn = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ cn.s ! Acc ! Sg ++ slash.post ;
    inf = slash.inf ++ cn.s ! Acc ! Sg ++ slash.post ;
    imp = \\pol,n => slash.imp ! pol ! n ++ cn.s ! Acc ! Sg ++ slash.post
  } ;
  FocusComp comp np = {
    s = \\t,pol => comp.s ! np.g ! np.n ++ np.s ! Nom
  } ;

  MkVPS temp pol vp = {s = \\g,n,p => vp.s ! temp.t ! pol.p ! g ! n ! p} ;
  BaseVPS x y = {s1=x.s; s2=y.s} ;
  ConsVPS x xs = {s1=\\g,n,p => x.s ! g ! n ! p ++ "," ++ xs.s1 ! g ! n ! p; s2=xs.s2} ;
  ConjVPS conj xs = {s = \\g,n,p => conj.s1 ++ xs.s1 ! g ! n ! p ++ conj.s2 ++ xs.s2 ! g ! n ! p} ;
  PredVPS np vps = {s = np.s ! Nom ++ vps.s ! np.g ! np.n ! np.p} ;
  SQuestVPS np vps = {s = np.s ! Nom ++ vps.s ! np.g ! np.n ! np.p} ;
  QuestVPS ip vps = {s = ip.s ! Nom ++ vps.s ! ip.g ! ip.n ! ip.p} ;
  RelVPS rp vps = {s = \\g,n => rp.s ! g ! n ! Nom ++ vps.s ! g ! n ! P3} ;

  ExistS temp pol np = {s = case pol.p of {R.Pos => "є" ++ np.s ! Nom ; R.Neg => "немає" ++ np.s ! Gen}} ;
  ExistNPQS temp pol np = {s = case pol.p of {R.Pos => "є" ++ np.s ! Nom ; R.Neg => "немає" ++ np.s ! Gen}} ;
  ExistIPQS temp pol ip = {s = "що є" ++ ip.s ! Nom} ;

  MkVPI vp = {s = vp.inf} ;
  BaseVPI x y = {s1=x.s; s2=y.s} ;
  ConsVPI x xs = {s1=x.s ++ "," ++ xs.s1; s2=xs.s2} ;
  ConjVPI conj xs = {s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2} ;
  ComplVPIVV vv vpi = {
    s = \\t,pol,g,n,p => finiteVerb vv t pol g n p ++ vpi.s ;
    inf = vv.infinitive ++ vpi.s ;
    imp = \\pol,n => neg pol ++ vv.imperative2 ! n ++ vpi.s
  } ;

  MkVPS2 temp pol slash = {s = \\g,n,p => slash.s ! temp.t ! pol.p ! g ! n ! p; c=slash.c; post=slash.post} ;
  BaseVPS2 x y = {s1=x.s; s2=y.s; c=y.c; post=y.post} ;
  ConsVPS2 x xs = {s1=\\g,n,p => x.s ! g ! n ! p ++ "," ++ xs.s1 ! g ! n ! p; s2=xs.s2; c=xs.c; post=xs.post} ;
  ConjVPS2 conj xs = {s = \\g,n,p => conj.s1 ++ xs.s1 ! g ! n ! p ++ conj.s2 ++ xs.s2 ! g ! n ! p; c=xs.c; post=xs.post} ;
  ComplVPS2 vps2 np = {s = \\g,n,p => vps2.s ! g ! n ! p ++ prepNP vps2.c np ++ vps2.post} ;
  ReflVPS2 vps2 rnp = {s = \\g,n,p => vps2.s ! g ! n ! p ++ prepNP vps2.c rnp ++ vps2.post} ;
  MkVPI2 slash = {s=slash.inf; c=slash.c; post=slash.post} ;
  BaseVPI2 x y = {s1=x.s; s2=y.s; c=y.c; post=y.post} ;
  ConsVPI2 x xs = {s1=x.s ++ "," ++ xs.s1; s2=xs.s2; c=xs.c; post=xs.post} ;
  ConjVPI2 conj xs = {s=conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2; c=xs.c; post=xs.post} ;
  ComplVPI2 vpi2 np = {s = vpi2.s ++ prepNP vpi2.c np ++ vpi2.post} ;

  BaseComp x y = {s1=x.s; s2=y.s} ;
  ConsComp x xs = {s1=\\g,n => x.s ! g ! n ++ "," ++ xs.s1 ! g ! n; s2=xs.s2} ;
  ConjComp conj xs = {s = \\g,n => conj.s1 ++ xs.s1 ! g ! n ++ conj.s2 ++ xs.s2 ! g ! n} ;
  BaseImp x y = {s1=x.s; s2=y.s} ;
  ConsImp x xs = {s1=\\pol,n => x.s ! pol ! n ++ "," ++ xs.s1 ! pol ! n; s2=xs.s2} ;
  ConjImp conj xs = {s = \\pol,n => conj.s1 ++ xs.s1 ! pol ! n ++ conj.s2 ++ xs.s2 ! pol ! n} ;

  ProDrop pron = pron ** {s = \\_ => []} ;
  ICompAP ap = {s = ap.s ! Nom ! GSg Neuter} ;
  IAdvAdv adv = adv ;
  CompIQuant iq = {s = iq.s ! Nom ! Masc ! Sg} ;
  PrepCN prep cn = {s = prep.s ++ cn.s ! prep.c ! Sg} ;
  FocusObj np sslash = {s = np.s ! Acc ++ sslash.s} ;
  FocusAdv adv s = {s = adv.s ++ s.s} ;
  FocusAdV adv s = {s = adv.s ++ s.s} ;
  PresPartAP vp = {s=\\c,gn => vp.inf} ;
  EmbedPresPart vp = {s = vp.inf} ;
  PastPartAP slash = {s=\\c,gn => slash.inf} ;
  PastPartAgentAP slash np = {s=\\c,gn => slash.inf ++ prepNP {s="ким"; c=Instr} np} ;
  PassVPSlash slash = {
    s = \\t,pol,g,n,p => copula t pol g n p ++ slash.inf ++ slash.post ;
    inf = "бути" ++ slash.inf ++ slash.post ;
    imp = \\pol,n => neg pol ++ "будь" ++ slash.inf ++ slash.post
  } ;
  PassAgentVPSlash slash np = {
    s = \\t,pol,g,n,p => copula t pol g n p ++ slash.inf ++ prepNP {s="ким"; c=Instr} np ++ slash.post ;
    inf = "бути" ++ slash.inf ++ prepNP {s="ким"; c=Instr} np ++ slash.post ;
    imp = \\pol,n => neg pol ++ "будь" ++ slash.inf ++ prepNP {s="ким"; c=Instr} np ++ slash.post
  } ;
  ProgrVPSlash slash = slash ;
  ExistsNP np = {s = \\t,pol => "існує" ++ np.s ! Nom} ;
  ComplBareVS vs s = {
    s = \\t,pol,g,n,p => finiteVerb vs t pol g n p ++ s.s ;
    inf = vs.infinitive ++ s.s ;
    imp = \\pol,n => neg pol ++ vs.imperative2 ! n ++ s.s
  } ;
  SlashBareV2S v s = {
    s = \\t,pol,g,n,p => finiteVerb v t pol g n p ;
    inf = v.infinitive ;
    imp = \\pol,n => neg pol ++ v.imperative2 ! n ;
    c = v.c2 ;
    post = s.s
  } ;
  ComplDirectVS vs utt = {
    s = \\t,pol,g,n,p => finiteVerb vs t pol g n p ++ utt.s ;
    inf = vs.infinitive ++ utt.s ;
    imp = \\pol,n => neg pol ++ vs.imperative2 ! n ++ utt.s
  } ;
  ComplDirectVQ vq utt = {
    s = \\t,pol,g,n,p => finiteVerb vq t pol g n p ++ utt.s ;
    inf = vq.infinitive ++ utt.s ;
    imp = \\pol,n => neg pol ++ vq.imperative2 ! n ++ utt.s
  } ;
  FrontComplDirectVS np vs utt = {s = \\t,pol => utt.s ++ np.s ! Nom ++ finiteVerb vs t pol np.g np.n np.p} ;
  FrontComplDirectVQ np vq utt = {s = \\t,pol => utt.s ++ np.s ! Nom ++ finiteVerb vq t pol np.g np.n np.p} ;
  PredAPVP ap vp = {s = \\t,pol => ap.s ! Nom ! GSg Neuter ++ vp.inf} ;
  AdjAsCN ap = constN (ap.s ! Nom ! GSg Masc) Masc ;
  AdjAsNP ap = {s=\\_=>ap.s ! Nom ! GSg Masc; g=Masc; n=Sg; p=P3} ;
  PredIAdvVP iadv vp = {s = \\t,pol => iadv.s ++ vp.inf} ;
  EmbedSSlash ss = {s = ss.s} ;

  ReflPron = {s = \\_ => "себе"} ;
  ReflPoss num cn = {s = \\c => "свій" ++ cn.s ! c ! num.n} ;
  PredetRNP pred rnp = {s = \\c => pred.s ! c ! Masc ! Sg ++ rnp.s ! c} ;
  AdvRNP np prep rnp = {s = \\c => np.s ! c ++ prepNP prep rnp} ;
  AdvRVP vp prep rnp = vp ** {
    s = \\t,pol,g,n,p => vp.s ! t ! pol ! g ! n ! p ++ prepNP prep rnp
  } ;
  AdvRAP ap prep rnp = ap ** {
    s = \\c,gn => ap.s ! c ! gn ++ prepNP prep rnp
  } ;
  PossPronRNP pron num cn rnp = {
    s = \\c => possPron pron.p pron.g pron.n cn.g num.n ++ cn.s ! c ! num.n ++ rnp.s ! Gen ;
    g = cn.g ;
    n = num.n ;
    p = P3
  } ;
  Base_rr_RNP x y = {s1=x.s; s2=y.s} ;
  Base_nr_RNP x y = {s1=x.s; s2=y.s} ;
  Base_rn_RNP x y = {s1=x.s; s2=y.s} ;
  Cons_rr_RNP x xs = {s1=\\c=>x.s ! c ++ "," ++ xs.s1 ! c; s2=xs.s2} ;
  Cons_nr_RNP x xs = {s1=\\c=>x.s ! c ++ "," ++ xs.s1 ! c; s2=xs.s2} ;
  ConjRNP conj xs = {s = \\c => conj.s1 ++ xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c} ;
  ReflPossPron = {s = \\_,g,n => possPron P3 Masc Sg g n} ;

  CompoundN n1 n2 = n2 ** {
    s = \\c,n => n1.s ! Nom ! Sg ++ n2.s ! c ! n ;
    voc = \\n => n1.s ! Nom ! Sg ++ n2.voc ! n
  } ;
  CompoundAP n a = {s=\\c,gn=>n.s ! Nom ! Sg ++ a.s ! c ! gn} ;
  GerundCN vp = constN vp.inf Neuter ;
  GerundNP vp = {s=\\_=>vp.inf; g=Neuter; n=Sg; p=P3} ;
  GerundAdv vp = {s=vp.inf} ;
  WithoutVP vp = {s="без" ++ vp.inf} ;
  ByVP vp = {s=vp.inf} ;
  ApposNP np app = np ** {s = \\c => np.s ! c ++ "," ++ app.s ! Nom} ;
  AdAdV ada adv = {s = ada.s ++ adv.s} ;
  UttAdV adv = adv ;
  PositAdVAdj a = {s = a.s ! Nom ! GSg Neuter} ;
  CompS s = {s=\\_,_=>s.s} ;
  CompQS qs = {s=\\_,_=>qs.s} ;
  UttVPShort vp = {s=vp.inf} ;
  ComplSlashPartLast slash np = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ prepNP slash.c np ++ slash.post ;
    inf = slash.inf ++ prepNP slash.c np ++ slash.post ;
    imp = \\pol,n => slash.imp ! pol ! n ++ prepNP slash.c np ++ slash.post
  } ;
  UseComp_estar comp = {
    s = \\t,pol,g,n,p => copula t pol g n p ++ comp.s ! g ! n ;
    inf = "бути" ++ comp.s ! Masc ! Sg ;
    imp = \\pol,n => neg pol ++ "будь" ++ comp.s ! Masc ! n
  } ;
  UseComp_ser = UseComp_estar ;
  SubjRelNP np rs = np ** {s=\\c=>np.s ! c ++ rs.s ! np.g ! np.n} ;
  UttAccNP np = {s=np.s ! Acc} ;
  UttDatNP np = {s=np.s ! Dat} ;
  UttAccIP ip = {s=ip.s ! Acc} ;
  UttDatIP ip = {s=ip.s ! Dat} ;
  UseDAP dap = {s=\\c=>dap.s ! c ! Masc; g=Masc; n=dap.n; p=P3} ;
  UseDAPMasc dap = {s=\\c=>dap.s ! c ! Masc; g=Masc; n=dap.n; p=P3} ;
  UseDAPFem dap = {s=\\c=>dap.s ! c ! Fem; g=Fem; n=dap.n; p=P3} ;
  CardCNCard card cn = {s=card.s ++ cn.s ! Nom ! Pl; n=Pl} ;
  TPastSimple = {s=[]; t=R.Past} ;
  SubjunctRelCN cn rs = cn ** {s=\\c,n=>cn.s ! c ! n ++ rs.s ! cn.g ! n} ;
}
