concrete ParseExtendSwa of ParseExtend =
 ExtendSwa - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
              GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
              CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
              UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
              PiedPipingQuestSlash, PiedPipingRelSlash],
 NumeralSwa - [num], PunctuationX ** open Prelude, ResSwa, GrammarSwa in {

lincat CNN = {s1,s2 : Str ; n1,n : Number ; g1,g2,g : Gender} ;

lin UttAP  p ap  = { s = ap.s ! getGender p.a ! getNumber p.a } ;
    UttVPS p vps = {s = vps.s ! agrFeatures p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarSwa.RelNP ;
    ExtRelNP = GrammarSwa.RelNP ;
    
lin BareN2 n = n ;

lin gen_Quant = {s = \\_,_ => []} ;
    that_RP = GrammarSwa.IdRP ;
    whatSgFem_IP = {s = \\_ => "nani" ; n = Sg} ;
    whatSgNeut_IP = {s = \\_ => "nini" ; n = Sg} ;

    BaseCNN n1 c1 n2 c2 = {
      s1 = c1.s ! n1.n ! Nom ++ c1.s2 ! n1.n ;
      s2 = c2.s ! n2.n ! Nom ++ c2.s2 ! n2.n ;
      n1 = n1.n ; n = n2.n ; g1 = c1.g ; g2 = c2.g ;
      g = conjGender c1.g c2.g
      } ;
    DetCNN q conj x = {
      s = \\_ => q.s ! x.n1 ! x.g ++ conj.s1 ++ x.s1 ++ conj.s2 ++ x.s2 ;
      a = Ag x.g Pl P3
      } ;
    ReflPossCNN conj x = {s = \\a =>
      conj.s1 ++ x.s1 ++ possessivePron a x.n1 x.g1 ++
      conj.s2 ++ x.s2 ++ possessivePron a x.n x.g2 ++ selfPron a ;
      isPron = False} ;
    PossCNN_RNP q conj x r = {s = \\a =>
      q.s ! x.n1 ! x.g ++ conj.s1 ++ x.s1 ++ conj.s2 ++ x.s2 ++
      possess_Prep.s ! Pl ! x.g ++ r.s ! a ;
      isPron = False} ;

    NumMore n = {s = \\g => n.s ! g ++ "zaidi" ; n = Pl} ;
    NumLess n = {s = \\g => n.s ! g ++ "chache" ; n = Pl} ;
    UseACard a = {s = \\_ => a.s ; n = Pl} ;
    UseAdAACard a c = {s = \\_ => a.s ++ c.s ; n = Pl} ;
    ExtAdvAP ap adv = {s = \\g,n => ap.s ! g ! n ++ SOFT_BIND ++ "," ++ adv.s} ;
    ComparAdv pol c a comp = {s = neg pol.p ++ c.s ++ a.s ++ c.p ++ comp.s ! Ag G1 Sg P3} ;
    CAdvAP pol c ap comp = {s = \\g,n => neg pol.p ++ c.s ++ ap.s ! g ! n ++ c.p ++ comp.s ! Ag g n P3} ;
    AdnCAdv pol c = {s = neg pol.p ++ c.s ++ c.p} ;
    EnoughAP ap ant pol vp = {s = \\g,n => ap.s ! g ! n ++ "vya kutosha" ++ infVP pol.p (agre g n P3) vp} ;
    EnoughAdv a = {s = a.s ++ "vya kutosha"} ;
    TimeNP np = {s = np.s ! NPAcc} ;
    AdvAdv a b = {s = a.s ++ b.s} ;

    EmbedVP ant pol pron vp = {s = infVP pol.p (agrFeatures pron.a) vp} ;
    ComplVV vv ant pol vp = insertObj (\\a => infVP pol.p a vp) (predV vv) ;
    SlashVV vv ant pol vp = insertObj (\\a => infVP pol.p a vp) (predV vv) ** {c2 = vp.c2} ;
    SlashV2V v ant pol vp = insertObj (\\a => infVP pol.p a vp) (predV v) ** {c2 = []} ;
    SlashV2VNP v np ant pol vp = insertObj (\\a => np.s ! NPAcc ++ infVP pol.p a vp) (predV v) ** {c2 = vp.c2} ;
    InOrderToVP ant pol pron vp = {s = "ili" ++ infVP pol.p (agrFeatures pron.a) vp} ;
    CompVP ant pol pron vp = {s = \\_ => infVP pol.p (agrFeatures pron.a) vp} ;
    UttVP ant pol pron vp = {s = infVP pol.p (agrFeatures pron.a) vp} ;

    ReflA2 a r = {s = \\g,n => a.s ! AAdj g n ++ a.c2 ++ r.s ! (Ag g n P3)} ;
    ReflVPSlash vp r = insertObj (\\a => vp.c2 ++ r.s ! (Ag a.g a.n a.p)) vp ;
    RecipVPSlash vp = insertObj (\\_ => vp.c2 ++ "wao kwa wao") vp ;
    RecipVPSlashCN vp cn = insertObj (\\a => vp.c2 ++ "wao kwa wao" ++ cn.s ! a.n ! Nom) vp ;
    FocusComp comp np = {s = \\_,_,_ => comp.s ! np.a ++ "ni" ++ np.s ! npNom} ;

oper neg : Polarity -> Str = \p -> case p of {Pos => [] ; Neg => "si"} ;

}
