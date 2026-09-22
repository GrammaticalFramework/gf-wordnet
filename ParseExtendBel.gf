--# -path=.:../gf-rgl/src/belarusian:../gf-rgl/src/abstract:../gf-rgl/src/common:prelude
concrete ParseExtendBel of ParseExtend =
  ExtendBel - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralBel - [num], PunctuationX **
  open ResBel, ParadigmsBel, (R = ParamX), Prelude in {

lincat
  CNN = {s1,s2 : Case => Str; g : Gender; n : Number} ;

lin
  gen_Quant = {s = \\_,_,_ => []} ;

  UttAP p ap = {s = p.s ! Nom ++ ap.s ! Nom ! genNum p.a.g p.a.n} ;
  UttVPS p vps = {s = vps.s ! p.a} ;
  PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

  ReflA2 a rnp = {s = \\c,gn => a.s ! c ! gn ++ a.c2.s ++ rnp.s ! a.c2.c; adv = a.adv ++ a.c2.s ++ rnp.s ! a.c2.c; post = a.post} ;
  ReflVPSlash vp rnp = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ rnp.s ! vp.c.c ++ vp.post ;
    inf = \\_ => vp.inf ++ rnp.s ! vp.c.c ;
    imp = \\p,n => vp.imp ! p ! n ++ rnp.s ! vp.c.c ++ vp.post
  } ;

  BaseCNN num1 cn1 num2 cn2 = {
    s1 = \\c => cn1.s ! c ! num1.n ;
    s2 = \\c => cn2.s ! c ! num2.n ;
    g = cn2.g ;
    n = num2.n
  } ;
  DetCNN quant conj cnn = {
    s = \\c => quant.s ! c ! cnn.g ! cnn.n ++ cnn.s1 ! c ++ conj.s ++ cnn.s2 ! c ;
    a = {g=cnn.g; n=cnn.n; p=P3}
  } ;
  ReflPossCNN conj cnn = {s = \\c =>
    (mkPossAdj "св").s ! c ! genNum cnn.g cnn.n ++ cnn.s1 ! c ++ conj.s ++ cnn.s2 ! c
  } ;
  PossCNN_RNP quant conj cnn rnp = {
    s = \\c => quant.s ! c ! cnn.g ! cnn.n ++ cnn.s1 ! c ++ conj.s ++ cnn.s2 ! c ++ rnp.s ! Gen
  } ;

  NumLess num = num ;
  NumMore num = num ;
  num n = n ;

  UseACard card = {s = card.s; n = card.n} ;
  UseAdAACard ada card = {s = ada.s ++ card.s; n = card.n} ;

  RelNP np rs = {s = \\c => np.s ! c ++ rs.s; a = np.a} ;
  ExtRelNP np rs = {s = \\c => np.s ! c ++ rs.s; a = np.a} ;
  ExtAdvAP ap adv = {s = \\c,gn => ap.s ! c ! gn ++ adv.s; adv = ap.adv ++ adv.s; post = ap.post} ;
  BareN2 n = n ;

  ComparAdv pol cadv adv comp = {
    s = neg pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! defaultAgr
  } ;
  CAdvAP pol cadv ap comp = {
    s = \\c,gn => neg pol.p ++ cadv.s ++ ap.s ! c ! gn ++ cadv.p ++ comp.s ! defaultAgr ;
    adv = neg pol.p ++ cadv.s ++ ap.adv ++ cadv.p ++ comp.s ! defaultAgr;
    post = ap.post
  } ;
  AdnCAdv pol cadv = {s = neg pol.p ++ cadv.s ++ cadv.p} ;

  EnoughAP ap ant pol vp = {
    s = \\c,gn => ap.s ! c ! gn ++ "дастаткова" ++ neg pol.p ++ vp.inf ! agrFromGenNum gn ;
    adv = ap.adv ++ "дастаткова" ++ neg pol.p ++ vp.inf ! defaultAgr;
    post = ap.post
  } ;
  EnoughAdv adv = {s = "дастаткова" ++ adv.s} ;
  TimeNP np = {s = np.s ! Acc} ;
  AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

  whatSgFem_IP = mkSimpleNP "што" Fem Sg P3 ;
  whatSgNeut_IP = mkSimpleNP "што" Neuter Sg P3 ;
  that_RP = {s = "які"} ;

  EmbedVP ant pol pron vp = {
    s = neg pol.p ++ vp.inf ! pron.a
  } ;
  ComplVV vv ant pol vp = {
    s = \\t,p,a => finiteVerb vv t p a ++ neg pol.p ++ vp.inf ! a ;
    inf = \\a => vv.infinitive ++ neg pol.p ++ vp.inf ! a ;
    imp = \\p,n => neg p ++ vv.imperative ! n ++ neg pol.p ++ vp.inf ! defaultAgr
  } ;
  SlashVV vv ant pol vp = {
    s = \\t,p,a => finiteVerb vv t p a ++ neg pol.p ++ vp.inf ;
    inf = vv.infinitive ++ neg pol.p ++ vp.inf ;
    c = vp.c ;
    imp = \\p,n => neg p ++ vv.imperative ! n ++ neg pol.p ++ vp.imp ! R.Pos ! n ;
    pass = vp.pass ;
    post = vp.post ;
    objPost = vp.objPost
  } ;
  SlashV2V v ant pol vp = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ neg pol.p ++ vp.inf ! defaultAgr ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = neg pol.p ++ vp.inf ! defaultAgr ;
    objPost = \\_ => neg pol.p ++ vp.inf ! defaultAgr
  } ;
  SlashV2VNP v np ant pol vp = {
    s = \\t,p,a => finiteVerb v t p a ++ prepNP v.c2 np ++ neg pol.p ++ vp.s ! R.Pres ! R.Pos ! np.a ;
    inf = v.infinitive ++ prepNP v.c2 np ++ neg pol.p ++ vp.inf ;
    c = vp.c ;
    imp = \\p,n => neg p ++ v.imperative ! n ++ prepNP v.c2 np ++ neg pol.p ++ vp.imp ! R.Pos ! n ;
    pass = vp.pass ;
    post = vp.post ;
    objPost = vp.objPost
  } ;
  InOrderToVP ant pol pron vp = {s = "каб" ++ neg pol.p ++ vp.inf ! pron.a} ;
  CompVP ant pol pron vp = {s = \\a => neg pol.p ++ vp.inf ! a} ;
  UttVP ant pol pron vp = {s = neg pol.p ++ vp.inf ! pron.a} ;

  RecipVPSlash vp = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ "адзін аднаго" ++ vp.post ;
    inf = \\_ => vp.inf ++ "адзін аднаго" ;
    imp = \\p,n => vp.imp ! p ! n ++ "адзін аднаго" ++ vp.post
  } ;
  RecipVPSlashCN vp cn = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ cn.s ! Acc ! Sg ++ vp.post ;
    inf = \\_ => vp.inf ++ cn.s ! Acc ! Sg ;
    imp = \\p,n => vp.imp ! p ! n ++ cn.s ! Acc ! Sg ++ vp.post
  } ;
  FocusComp comp np = {s = \\t,p => comp.s ! np.a ++ np.s ! Nom} ;

}
