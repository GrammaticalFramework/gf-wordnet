--# -path=.:../gf-rgl/src/ukrainian:../gf-rgl/src/abstract:../gf-rgl/src/common:prelude
concrete ParseExtendUkr of ParseExtend =
  ExtendUkr - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralUkr - [num], PunctuationX **
  open ResUkr, (R = ParamX), Prelude in {

lincat
  CNN = {s : Case => Number => Str; g : Gender; n : Number} ;

lin
  gen_Quant = {s = \\_,_,_ => []} ;

  UttAP p ap = {s = ap.s ! Nom ! genNum p.g p.n} ;
  UttVPS p vps = {s = vps.s ! p.g ! p.n ! p.p} ;
  PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
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
    s = \\c => "його" ++ cnn.s ! c ! cnn.n
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

}
