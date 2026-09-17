concrete ParseExtendSqi of ParseExtend =
  ExtendSqi - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
               GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
               UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralSqi - [num], PunctuationX **
  open Prelude, ParamX, ResSqi, GrammarSqi, (I=IrregSqi) in {

lincat
  CNN = {s1,s2 : Species=>Case=>Str; g1,g2:Gender; n1,n2:Number} ;

oper
  infVP : Ant -> Pol -> Agr -> VP -> Str = \ant,pol,a,vp -> case ant.a of {
    Simul => "të" ++ case pol.p of {Pos=>[];Neg=>"mos"} ++
      vp.subjunctive!agrNumber a!a.p!agrGender a!Nom;
    Anter => "të" ++ case pol.p of {Pos=>[];Neg=>"mos"} ++ haveAux!ParamX.Pres!agrNumber a!a.p++vp.participle!a!Nom
  } ;

lin
  num x=x ;
  gen_Quant=DefArt ;
  that_RP=IdRP ;
  UttAP p ap={s=ap.s!Indef!Nom!agrGender p.a!agrNumber p.a} ;
  UttVPS p v={s=v.s!p.a} ;
  UttVP ant pol p vp={s=infVP ant pol p.a vp} ;
  EmbedVP ant pol p vp={s=infVP ant pol p.a vp} ;
  CompVP ant pol p vp={s=\\_=>infVP ant pol p.a vp} ;
  PhrUttMark pc u voc mark={s=CAPIT++pc.s++u.s++voc.s++SOFT_BIND++mark.s} ;

  BaseCNN n1 c1 n2 c2={
    s1=\\sp,c=>n1.s++c1.s ! sp ! c ! n1.n;
    s2=\\sp,c=>n2.s++c2.s ! sp ! c ! n2.n;
    g1=c1.g;
    g2=c2.g;
    n1=n1.n;
    n2=n2.n
  };
  DetCNN q conj x= {
    s = \\c => q.s!c!x.g1!x.n1++x.s1!q.sp!c++conj.s++q.s!c!x.g2!x.n2++x.s2!q.sp!c;
    a = {gn=GPl;p=P3}
  };
  ReflPossCNN conj x={s=\\_,k=>x.s1!Def!k++conj.s++x.s2!Def!k;isPron=False};
  PossCNN_RNP q conj x r={s=\\a,k=>q.s!k!x.g1!x.n1++x.s1!q.sp!k++conj.s++q.s!k!x.g2!x.n2++x.s2!q.sp!k++link_clitic!q.sp!k!x.g2!x.n2++r.s!a!Ablat;isPron=False};

  NumMore n={s=n.s++"më shumë";n=Pl}; NumLess n={s=n.s++"më pak";n=Pl};
  UseACard c={s=c.s}; UseAdAACard a c={s=a.s++c.s};
  RelNP np rs=np**{s=\\k=>np.s ! k++rs.s!np.a}; ExtRelNP=RelNP;
  BareN2 n = n;
  ComparAdv pol c a comp={s=negation pol.p++c.s++a.s++c.p++comp.s!{gn=GSg Masc;p=P3}};
  CAdvAP pol c ap comp= {
    s = \\sp,k,g,n=>negation pol.p++c.s++ap.s ! sp ! k ! g ! n++c.p++comp.s!agrgP3 g n
  };
  AdnCAdv pol c={s=negation pol.p++c.s++c.p};
  EnoughAP ap ant pol vp={s=\\sp,k,g,n=>ap.s ! sp ! k ! g ! n++"mjaftueshëm"++infVP ant pol (agrgP3 g n) vp};
  EnoughAdv a={s=a.s++"mjaftueshëm"};
  ExtAdvAP ap adv={s=\\sp,c,g,n=>ap.s ! sp ! c ! g ! n++SOFT_BIND++","++adv.s};
  AdvAdv a b={s=a.s++b.s};
  whatSgFem_IP={s="çfarë";a={gn=GSg Fem;p=P3}};
  whatSgNeut_IP={s="çfarë";a={gn=GSg Masc;p=P3}};

  ComplVV vv ant pol vp=extAddVP (baseVP vv) (\\a=>infVP ant pol a vp);
  SlashVV vv ant pol sl=sl**{
    indicative=\\t,n,p=>vv.indicative!t!n!p++infVP ant pol {gn=case n of {Sg=>GSg Masc;Pl=>GPl};p=p} (baseSlashVP sl);
    subjunctive=\\n,p=>subjunctiveForm vv.vtype
      (vv.indicative!ResSqi.Pres!Sg!P1)
      (vv.indicative!ResSqi.Pres!n!p) n p++
      infVP ant pol {gn=case n of {Sg=>GSg Masc;Pl=>GPl};p=p} (baseSlashVP sl)
    };
  SlashV2V v ant pol vp= v ** {
    indicative=\\t,n,p=>v.indicative!t!n!p++infVP ant pol {gn=case n of {Sg=>GSg Masc;Pl=>GPl};p=p} vp;
    subjunctive=\\n,p=>subjunctiveForm v.vtype
      (v.indicative!ResSqi.Pres!Sg!P1)
      (v.indicative!ResSqi.Pres!n!p) n p++
      infVP ant pol {gn=case n of {Sg=>GSg Masc;Pl=>GPl};p=p} vp;
    imperative=\\n=>v.imperative!n++infVP ant pol {gn=case n of {Sg=>GSg Masc;Pl=>GPl};p=P2} vp
    };
  SlashV2VNP v np ant pol sl=sl**{
    indicative=\\t,n,p=>v.indicative!t!n!p++v.c2.s++np.s ! v.c2.c++infVP ant pol np.a (baseSlashVP sl);
    subjunctive=\\n,p=>subjunctiveForm v.vtype
      (v.indicative!ResSqi.Pres!Sg!P1)
      (v.indicative!ResSqi.Pres!n!p) n p++
      v.c2.s++np.s ! v.c2.c++infVP ant pol np.a (baseSlashVP sl)
    };
  InOrderToVP ant pol p vp={s="për të"++infVP ant pol p.a vp};

  ReflA2 a r= {
    s=\\sp,c,g,n=>(PositA a).s ! sp ! c ! g ! n++a.c2.s++r.s!(agrgP3 g n)!a.c2.c};
  ReflVPSlash sl r=extAddVP (baseSlashVP sl) (\\a=>sl.c2.s++r.s!a!sl.c2.c);
  RecipVPSlash sl=extAddVP (baseSlashVP sl) (\\_=>sl.c2.s++"njëri-tjetrin");
  RecipVPSlashCN sl cn=extAddVP (baseSlashVP sl) (\\_=>sl.c2.s++"njëri"++cn.s!Indef!Acc!Sg++"tjetrin");
  FocusComp comp np=PredVP np (UseComp comp);
  TimeNP np={s=np.s ! Acc};
}
