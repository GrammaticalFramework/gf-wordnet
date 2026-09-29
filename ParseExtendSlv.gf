concrete ParseExtendSlv of ParseExtend =
  ExtendSlv - [iFem_Pron, youPolFem_Pron, youPolPlFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralSlv - [num], PunctuationX ** 
  open Prelude, ResSlv, ParadigmsSlv in {

lincat CNN = {s : Species => Case => Str; a : Agr} ;

lin
    UttAP p ap = {s = ap.s ! Indef ! inanimateGender p.a.g ! Nom ! p.a.n} ;

    gen_Quant = {s=\\_,_,_=>[];spec=Indef} ;
    UttVPS p vps = {s=vps.s ! p.a} ;

    ReflA2 = ExtendSlv.ReflA2RNP ;
    ReflVPSlash = ExtendSlv.ReflRNP ;

    BaseCNN na a nb b = {
      s=\\sp,c => a.s!sp!c!(numAgr2num!na.n) ++ "in" ++ b.s!sp!c!(numAgr2num!nb.n);
      a={g=agender2gender b.g;n=Pl;p=P3}
    } ;
    DetCNN q conj cnn = {s=\\c=>q.s!cnn.a.g!c!cnn.a.n++cnn.s!q.spec!c;a=cnn.a;isPron=False} ;
    ReflPossCNN conj cnn = {s=\\a,c => "svoj" ++ cnn.s!Indef!c} ;
    PossCNN_RNP q conj cnn r = {s=\\a,c=>q.s!a.g!c!a.n++cnn.s!q.spec!c++conj.s++r.s!a!c} ;

    NumLess n = n ** {s=\\g,c => n.s!g!c ++ "manj"} ;
    NumMore n = n ** {s=\\g,c => n.s!g!c ++ "več"} ;
    UseACard c = {s=\\_,_=>c.s;n=UseNum Pl} ;
    UseAdAACard a c = {s=\\_,_=>a.s++c.s;n=UseNum Pl} ;

    RelNP np rs = np ** {s=\\c=>np.s!c++rs.s!np.a} ;
    ExtRelNP np rs = np ** {s=\\c=>np.s!c++","++rs.s!np.a} ;
    ExtAdvAP ap adv = ap ** {s=\\sp,g,c,n=>ap.s!sp!g!c!n++","++adv.s} ;
    ComparAdv pol cadv adv comp = {s=pol.s++cadv.s++adv.s++cadv.p++comp.s!{g=Neut;n=Sg;p=P3}} ;
    CAdvAP pol cadv ap comp = ap ** {s=\\sp,g,c,n=>pol.s++cadv.s++ap.s!sp!g!c!n++cadv.p++comp.s!{g=agender2gender g;n=n;p=P3}} ;
    AdnCAdv pol cadv = {s=pol.s++cadv.s} ;
    EnoughAP ap ant pol vp = ap ** {s=\\sp,g,c,n=>ap.s!sp!g!c!n++"dovolj, da"++vp.s!pol.p!VInf++vp.s2!{g=agender2gender g;n=n;p=P3}} ;
    EnoughAdv adv = {s=adv.s++"dovolj"} ;
    TimeNP np = {s=np.s!Acc} ;
    AdvAdv a b = {s=a.s++b.s} ;
    whatSgFem_IP = {s=\\_=>"kaj";a={g=Fem;n=Sg;p=P3}} ;
    whatSgNeut_IP = {s=\\_=>"kaj";a={g=Neut;n=Sg;p=P3}} ;
    that_RP = {s=\\_,_,_=>"ki"} ;

    EmbedVP ant pol pron vp = {s=vp.s!pol.p!VInf++vp.s2!pron.a} ;
    ComplVV vv ant pol vp = {s=\\p,vf=>ne!p++vv.s!vf;s2=\\a=>vp.s!pol.p!VInf++vp.s2!a;isCop=False;refl=[]} ;
    SlashVV vv ant pol vp = vp ** {s=\\p,vf=>ne!p++vv.s!vf;s2=\\a=>vp.s!pol.p!VInf++vp.s2!a} ;
    SlashV2V v ant pol vp = {s=\\p,vf=>ne!p++v.s!vf;s2=\\a=>vp.s!pol.p!VInf++vp.s2!a;c2=mkPrep [] accusative;isCop=False;refl=v.refl} ;
    SlashV2VNP v np ant pol vp = vp ** {s=\\p,vf=>ne!p++v.s!vf;s2=\\a=>np.s!Acc++vp.s!pol.p!VInf++vp.s2!a} ;
    InOrderToVP ant pol pron vp = {s="da bi"++vp.s!pol.p!VInf++vp.s2!pron.a} ;
    CompVP ant pol pron vp = {s=\\_=>vp.s!pol.p!VInf++vp.s2!pron.a} ;
    UttVP ant pol pron vp = {s=vp.s!pol.p!VInf++vp.s2!pron.a} ;
    RecipVPSlash vp = vp ** {s2=\\a=>vp.s2!a++"drug drugega"} ;
    RecipVPSlashCN vp cn = vp ** {s2=\\a=>vp.s2!a++"drug drugega"++cn.s!Indef!Acc!a.n} ;
    FocusComp comp np = mkClause (np.s!Nom) np.a np.isPron {s=copula;s2=comp.s;isCop=True;refl=[]} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;

}
