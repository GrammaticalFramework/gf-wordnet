concrete ParseExtendKaz of ParseExtend =
  ExtendKaz - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
               GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, UncontractedNeg,
               AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralKaz - [num],
  PunctuationX **
  open Prelude, ResKaz, ParadigmsKaz, GrammarKaz in {

  lincat CNN = {s : Case => Str; n : Number} ;

  lin
    gen_Quant = {s=[]; poss=NoPoss} ;
    UttAP pron ap = ap ;
    UttVPS pron vps = {s=pron.s ! Nom ++ vps.s} ;
    PhrUttMark p u v m = {s=p.s ++ u.s ++ v.s ++ m.s} ;
    BareN2 n = n ;
    RelNP np rs = {s=\\c => np.s ! c ++ rs.s; a=np.a} ;
    ExtRelNP np rs = {s=\\c => np.s ! c ++ "," ++ rs.s; a=np.a} ;
    ExtAdvAP ap adv = {s=ap.s ++ "," ++ adv.s} ;
    BaseCNN n1 c1 n2 c2 = {s=\\c => c1.s ! c ! n1.n ++ c2.s ! c ! n2.n; n=Pl} ;
    DetCNN q conj cnn = {s=\\c => q.s ++ cnn.s ! c; a={p=P3;n=cnn.n}} ;
    ReflPossCNN conj cnn = {s=\\c => cnn.s ! c} ;
    PossCNN_RNP q conj cnn r = {s=\\c => q.s ++ cnn.s ! c ++ r.s ! c} ;
    ReflVPSlash slash r = prefixVerb (r.s ! slash.c2.c ++ slash.c2.s) slash ;
    ReflA2 a r = {s=r.s ! a.c2.c ++ a.c2.s ++ a.s} ;
    NumLess n = {s=n.s ++ "кем";n=n.n} ; NumMore n = {s=n.s ++ "артық";n=n.n} ;
    UseACard c = {s=c.s} ; UseAdAACard a c = {s=a.s ++ c.s} ;
    ComparAdv pol ca adv comp = {s=pol.s ++ ca.s ++ adv.s ++ ca.p ++ comp.s} ;
    CAdvAP pol ca ap comp = {s=pol.s ++ ca.s ++ ap.s ++ ca.p ++ comp.s} ;
    AdnCAdv pol ca = {s=pol.s ++ ca.s ++ ca.p} ;
    EnoughAP ap ant pol vp = {s=ap.s ++ "жеткілікті" ++ pol.s ++ vp.infinitive} ;
    EnoughAdv adv = {s=adv.s ++ "жеткілікті"} ;
    TimeNP np = {s=np.s ! Nom} ; AdvAdv a b = {s=a.s ++ b.s} ;
    whatSgFem_IP={s="не"}; whatSgNeut_IP={s="не"}; that_RP={s=""};
    EmbedVP ant pol pron vp = {s=pron.s ! Nom ++ pol.s ++ vp.infinitive} ;
    ComplVV vv ant pol vp = prefixVerb (pol.s ++ vp.infinitive) vv ;
    SlashVV vv ant pol slash = prefixVerb (pol.s ++ slash.infinitive) vv ** {c2=slash.c2} ;
    SlashV2V v ant pol vp = prefixVerb (pol.s ++ vp.infinitive) v ** {c2=v.c2} ;
    SlashV2VNP v np ant pol slash =
      (prefixVerb (np.s ! v.c2.c ++ v.c2.s ++ pol.s ++ slash.infinitive) v ** {c2=v.c3}) ;
    InOrderToVP ant pol pron vp = {s=pron.s ! Nom ++ pol.s ++ vp.infinitive ++ "үшін"} ;
    CompVP ant pol pron vp = {s=pron.s ! Nom ++ pol.s ++ vp.infinitive} ;
    UttVP ant pol pron vp = {s=pron.s ! Nom ++ pol.s ++ vp.infinitive} ;
    RecipVPSlash slash = prefixVerb "бір-бірін" slash ;
    RecipVPSlashCN slash cn = prefixVerb (cn.s ! Acc ! Pl) slash ;
    FocusComp comp np = {
      pres = \\_ => comp.s ++ np.s ! Nom ;
      past = \\_ => comp.s ++ np.s ! Nom ;
      fut = \\_ => comp.s ++ np.s ! Nom ;
      cond = \\_ => comp.s ++ np.s ! Nom ;
      anter = \\_ => comp.s ++ np.s ! Nom
      } ;
    num n = {s=n.s} ;
}
