concrete ParseExtendLat of ParseExtend =
  ExtendLat - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
               GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
               UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralLat, PunctuationX **
  open Prelude, ResLat, (G=GrammarLat), (N=NounLat), (E=ExtendLat) in {

lincat CNN = {s1,s2 : Case => Str; n1,n2 : Number; g1,g2 : Gender} ;

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    UttAP p ap = ss (ap.s ! Ag p.pers.g p.pers.n Nom );
    UttVPS p vps = ss (vps.s ! p.pers.g ! p.pers.n ! p.p) ;

    gen_Quant = {s,sp = \\_ => ""} ;
    RelNP np rs = np ** {adv = rs.s ! np.g ! np.n ++ np.adv} ;
    ExtRelNP = N.RelNP ;
    BareN2 n = n ;

    BaseCNN num1 cn1 num2 cn2 = {
      s1=\\c=>cn1.s ! num1.n ! c; s2=\\c=>cn2.s ! num2.n ! c;
      n1=num1.n;n2=num2.n;g1=cn1.g;g2=cn2.g
      } ;
    DetCNN quant conj cnn = {
      s=\\_,c=>quant.s ! Ag cnn.g1 cnn.n1 c ++ conj.s1 ++ cnn.s1 ! c ++
        conj.s2 ++ cnn.s2 ! c ++ conj.s3;
      n=Pl;g=cnn.g1;p=P3;adv="";preap,postap={s=\\_=>""};
      det={s,sp=\\_=>"";n=Pl}
      } ;
    ReflPossCNN conj cnn = {s=\\_,c=>(createPronouns Masc Sg P3).p2 ! PronRefl ! Ag cnn.g1 cnn.n1 c ++
      conj.s1 ++ cnn.s1 ! c ++ conj.s2 ++ cnn.s2 ! c ++ conj.s3} ;
    PossCNN_RNP quant conj cnn rnp = {s=\\a,c=>quant.s ! Ag cnn.g1 cnn.n1 c ++
      conj.s1 ++ cnn.s1 ! c ++ conj.s2 ++ cnn.s2 ! c ++ conj.s3 ++ rnp.s ! a ! Gen} ;

    NumMore n = n ;
    NumLess n = n ;
    UseACard card = {s=\\_,_=>card.s;n=Pl} ;
    UseAdAACard ada card = {s=\\_,_=>ada.s ++ card.s;n=Pl} ;

    ExtAdvAP ap adv = {s=\\a=>ap.s ! a ++ bindComma ++ adv.s ! Posit} ;
    TimeNP np = mkAdverb (combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Acc) ;
    AdvAdv a b = mkAdverb (a.s ! Posit ++ b.s ! Posit) ;
    whatSgFem_IP = {s=\\c=>(G.whatSg_IP).s ! c;n=Sg} ;
    whatSgNeut_IP = {s=\\c=>(G.whatSg_IP).s ! c;n=Sg} ;
    ComparAdv pol cadv adv comp = mkAdverb (pol.s ++ cadv.s ++ adv.s ! Posit ++ cadv.p ++ comp.s ! Ag Neutr Sg Nom) ;
    CAdvAP pol cadv ap comp = {s=\\a=>pol.s ++ cadv.s ++ ap.s ! a ++ cadv.p ++ comp.s ! a} ;
    AdnCAdv pol cadv = {s=pol.s ++ cadv.s ++ cadv.p} ;
    EnoughAP ap ant pol vp = {s=\\a=>ap.s ! a ++ "satis" ++ pol.s ++ vp.obj ++ vp.compl ! a ++ vp.inf ! VInfActPres} ;
    EnoughAdv adv = mkAdverb (adv.s ! Posit ++ "satis") ;

    EmbedVP ant pol pron vp = {s=pol.s ++ vp.obj ++ vp.compl ! Ag pron.pers.g pron.pers.n Nom ++ vp.inf ! VInfActPres ++ vp.adv} ;
    ComplVV vv ant pol vp = (predV vv) ** {compl=\\a=>pol.s ++ vp.obj ++ vp.compl ! a ++ vp.inf ! VInfActPres ++ vp.adv} ;
    SlashVV vv ant pol vp = (predV vv) ** {compl=\\a=>pol.s ++ vp.obj ++ vp.compl ! a ++ vp.inf ! VInfActPres ++ vp.adv;c=vp.c} ;
    SlashV2V v ant pol vp = (predV v) ** {compl=\\a=>pol.s ++ vp.obj ++ vp.compl ! a ++ vp.inf ! VInfActPres ++ vp.adv;c=mkPreposition "" Acc} ;
    SlashV2VNP v np ant pol vp = (predV v) ** {
      obj=combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Acc;
      compl=\\a=>pol.s ++ vp.obj ++ vp.compl ! a ++ vp.inf ! VInfActPres ++ vp.adv;c=vp.c} ;
    InOrderToVP ant pol pron vp = mkAdverb ("ut" ++ pol.s ++ vp.obj ++ vp.compl ! Ag pron.pers.g pron.pers.n Nom ++ vp.inf ! VInfActPres ++ vp.adv) ;
    CompVP ant pol pron vp = {s=\\_=>pol.s ++ vp.obj ++ vp.compl ! Ag pron.pers.g pron.pers.n Nom ++ vp.inf ! VInfActPres ++ vp.adv} ;
    UttVP ant pol pron vp = ss (pol.s ++ vp.obj ++ vp.compl ! Ag pron.pers.g pron.pers.n Nom ++ vp.inf ! VInfActPres ++ vp.adv) ;

    ReflVPSlash = E.ReflRNP ;
    RecipVPSlash vp = vp ** {compl=\\_=>"inter se"} ;
    RecipVPSlashCN vp cn = vp ** {compl=\\a=>"inter" ++ cn.s ! Pl ! Acc} ;
    ReflA2 = E.ReflA2RNP ;
    FocusComp comp np = mkClause np (insertAdj comp.s (predV esseAux)) ;

lin that_RP = G.IdRP ;

}
