concrete ParseExtendHye of ParseExtend =
  ExtendHye - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralHye - [num], PunctuationX **
  open Prelude, ResHye, ParadigmsHye in {

lincat
  CNN = {s : Case => Str; n : Number} ;

oper
  simpleCl : Str -> Cl = \s -> lin Cl {s=s;negative=table {_=>"չ"++s};conditional=\\_,_=>s;
    converb={imperfective=s;futCon1=s;futCon2=s;negative=s;perfective=s;simultaneous=s};
    passive=s;past=\\_,_=>s;participle=\\_=>s;subjunctive=\\_,_=>s} ;

lin
  gen_Quant = {s=[];sp=Indef} ;
  UttAP pron ap = {s=ap.s ! Indef ! Nom ! pron.a.n} ;
  UttVPS pron vps = {s=pron.s!Nom ++ vps.s} ;
  PhrUttMark p u v m = {s=p.s ++ u.s ++ v.s ++ m.s} ;
  ReflA2 a r = {s=\\sp,c,n=>a.s!c!n ++ a.c2.s ++ r.s!a.c2.c;isPre=False} ;
  ReflVPSlash slash r = extAddVP (extToVP slash) (slash.c2.s ++ r.s!slash.c2.c) ;
  BaseCNN n1 c1 n2 c2 = {s=\\c=>c1.s!Indef!c!n1.n ++ c2.s!Indef!c!n2.n;n=Pl} ;
  DetCNN q conj cnn = {s=\\c=>q.s ++ cnn.s!c;a={n=cnn.n;p=P3}} ;
  ReflPossCNN conj cnn = {s=\\c=>cnn.s!c ++ "իրենց"} ;
  PossCNN_RNP q conj cnn r = {s=\\c=>q.s ++ cnn.s!c ++ r.s!Dat} ;
  NumLess n = {s=n.s ++ "պակաս";n=n.n} ;
  NumMore n = {s=n.s ++ "ավելի";n=n.n} ;
  num n = {s=n.s;n=n.n} ;
  UseACard c = {s=c.s} ;
  UseAdAACard a c = {s=a.s ++ c.s} ;
  RelNP np rs = {s=\\c=>np.s!c ++ rs.s;a=np.a} ;
  ExtRelNP np rs = {s=\\c=>np.s!c ++ "," ++ rs.s;a=np.a} ;
  ExtAdvAP ap adv = {s=\\sp,c,n=>ap.s!sp!c!n ++ "," ++ adv.s;isPre=ap.isPre} ;
  BareN2 n = n ;
  ComparAdv pol ca adv comp = {s=pol.s ++ ca.s ++ adv.s ++ ca.p ++ comp.s} ;
  CAdvAP pol ca ap comp = {s=\\sp,c,n=>pol.s++ca.s++ap.s!sp!c!n++ca.p++comp.s;isPre=True} ;
  AdnCAdv pol ca = {s=pol.s ++ ca.s ++ ca.p} ;
  EnoughAP ap ant pol vp = {s=\\sp,c,n=>ap.s!sp!c!n++"բավական"++pol.s++vp.s;isPre=ap.isPre} ;
  EnoughAdv adv = {s=adv.s ++ "բավական"} ;
  TimeNP np = {s=np.s!Nom} ;
  AdvAdv a b = {s=a.s++b.s} ;
  whatSgFem_IP = {s="ինչ"} ;
  whatSgNeut_IP = {s="ինչ"} ;
  that_RP = {s="որը"} ;
  EmbedVP ant pol pron vp = {s=pol.s ++ vp.s} ;
  ComplVV vv ant pol vp = extAddVP (extToVP vv) (pol.s ++ vp.s) ;
  SlashVV vv ant pol slash = slash ** {s=vv.s++pol.s++slash.s} ;
  SlashV2V v ant pol vp = v ** {s=v.s++pol.s++vp.s;c2=v.c2} ;
  SlashV2VNP v np ant pol slash = slash ** {s=v.s++v.c2.s++np.s!v.c2.c++pol.s++slash.s} ;
  InOrderToVP ant pol pron vp = {s="որպեսզի" ++ pol.s ++ vp.s} ;
  CompVP ant pol pron vp = {s=pol.s ++ vp.s} ;
  UttVP ant pol pron vp = {s=pol.s ++ vp.s} ;
  RecipVPSlash slash = extAddVP (extToVP slash) "միմյանց" ;
  RecipVPSlashCN slash cn = extAddVP (extToVP slash) (cn.s!Indef!Nom!Pl) ;
  FocusComp comp np = simpleCl (comp.s ++ np.s!Nom) ;
}
