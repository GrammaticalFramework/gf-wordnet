concrete ParseExtendFao of ParseExtend =
  ExtendFao - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron],
  NumeralFao - [num], PunctuationX ** open Prelude, ResFao, (P = ParamX) in {

lincat
  CNN = {s : Species => Case => Str ; n : Number ; g : Gender} ;

lin UttAP  p ap  = {s = ap.s ! Strong ! p.g ! p.n ! Nom} ;
    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;
    gen_Quant = {s = \\_,_,_,_ => [] ; sp = Indef ; d = Strong} ;

    UttVPS p vps = {s = vps.s ! p.g ! persNum p.n p.p} ;
    ReflA2 a rnp = {
      s = \\d,g,n,c => a.s ! d ! g ! n ! c ++ a.c2.s ++ rnp.s ! a.c2.c ;
    } ;
    ReflVPSlash vps rnp = {
      Converb = vps.Converb ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Imperative = \\n => vps.imperative ! n ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Indicative = \\t,pol,g,p => vps.Indicative ! t ! p ++ vps.particle ++ negStr pol ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Finite = vps.Indicative ;
      Remainder = \\pol,_,_ => negStr pol ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Nonfinite = vps.Nonfinite ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc ;
      Participle = \\t => vps.Participle ! t ++ vps.particle ++ vps.c2.s ++ rnp.s ! vps.c2.c ++ vps.sc
    } ;
    BaseCNN n1 cn1 n2 cn2 = {
      s = \\sp,c => cn1.s ! sp ! n1.n ! c ++ cn2.s ! sp ! n2.n ! c ;
      n = Pl ;
      g = cn2.g
    } ;
    DetCNN quant conj cnn = mkNP (quant.s ! False ! cnn.g ! cnn.n ! Nom ++ cnn.s ! quant.sp ! Nom) cnn.g cnn.n P3 ;
    ReflPossCNN conj cnn = mkNP (cnn.s ! Def ! Nom) cnn.g cnn.n P3 ;
    PossCNN_RNP quant conj cnn rnp = mkNP (quant.s ! False ! cnn.g ! cnn.n ! Nom ++ cnn.s ! quant.sp ! Nom ++ rnp.s ! Gen) cnn.g cnn.n P3 ;

    InOrderToVP ant pol pron vp = {s = "fyri at" ++ negStr pol.p ++ vp.Nonfinite} ;
    CompVP ant pol pron vp = {s = \\_,_ => negStr pol.p ++ vp.Nonfinite} ;
    UttVP ant pol pron vp = {s = negStr pol.p ++ vp.Nonfinite} ;

    NumLess num = num ** {s = \\g,c => num.s ! g ! c ++ "minni"} ;
    NumMore num = num ** {s = \\g,c => num.s ! g ! c ++ "afturat"} ;
    UseACard acard = {s = \\_,_ => acard.s ; n = Pl} ;
    UseAdAACard ada acard = {s = \\_,_ => ada.s ++ acard.s ; n = Pl} ;
    RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s ! np.g ! persNum np.n np.p} ;
    ExtRelNP np rs = np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.g ! persNum np.n np.p} ;
    ExtAdvAP ap adv = {s = \\d,g,n,c => ap.s ! d ! g ! n ! c ++ "," ++ adv.s} ;
    BareN2 n2 = n2 ;
    ComparAdv pol cadv adv comp = {s = negStr pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! Masc ! Sg} ;
    CAdvAP pol cadv ap comp = {s = \\d,g,n,c => negStr pol.p ++ cadv.s ++ ap.s ! d ! g ! n ! c ++ cadv.p ++ comp.s ! g ! n} ;
    AdnCAdv pol cadv = {s = negStr pol.p ++ cadv.s} ;
    EnoughAP ap ant pol vp = {s = \\d,g,n,c => ap.s ! d ! g ! n ! c ++ "nóg" ++ negStr pol.p ++ vp.Nonfinite} ;
    EnoughAdv adv = {s = adv.s ++ "nóg"} ;
    TimeNP np = {s = np.s ! Acc} ;
    AdvAdv a b = {s = a.s ++ b.s} ;
    whatSgFem_IP = {s = "hvat" ; n = Sg} ;
    whatSgNeut_IP = {s = "hvat" ; n = Sg} ;
    that_RP = {s = "sum"} ;
    EmbedVP ant pol pron vp = {s = negStr pol.p ++ vp.Nonfinite} ;
    ComplVV vv ant pol vp = {
      Converb = vv.Converb ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite ;
      Imperative = \\n => vv.imperative ! n ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite ;
      Indicative = \\t,p2,g,pn => vv.Indicative ! t ! pn ++ vv.particle ++ negStr p2 ++ negStr pol.p ++ vp.Nonfinite ;
      Finite = vv.Indicative ;
      Remainder = \\p2,_,_ => negStr p2 ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite ;
      Nonfinite = vv.Nonfinite ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite ;
      Participle = \\t => vv.Participle ! t ++ vv.particle ++ negStr pol.p ++ vp.Nonfinite
    } ;
    SlashVV vv ant pol vps = vps ** {
      Converb = vv.Converb ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite ;
      Indicative = \\t,pn => vv.Indicative ! t ! pn ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite ;
      Nonfinite = vv.Nonfinite ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite ;
      Participle = \\t => vv.Participle ! t ++ vv.particle ++ negStr pol.p ++ vps.Nonfinite
    } ;
    SlashV2V v ant pol vp = v ** {
      c2 = v.c2 ;
      sc = negStr pol.p ++ vp.Nonfinite
    } ;
    SlashV2VNP v np ant pol vps = v ** {
      c2 = v.c2 ;
      sc = v.c3.s ++ np.s ! v.c3.c ++ negStr pol.p ++ vps.Nonfinite
    } ;
    RecipVPSlash vps = mkVP (vps.Nonfinite ++ "hvør annan") ;
    RecipVPSlashCN vps cn = mkVP (vps.Nonfinite ++ cn.s ! Def ! Sg ! Acc) ;
    FocusComp comp np = {
      Converb = comp.s ! np.g ! np.n ++ copula ! Pres ! persNum np.n np.p ++ np.s ! Nom ;
      Indicative = \\t,pol => comp.s ! np.g ! np.n ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ np.s ! Nom ;
      Interrogative = \\t,pol => comp.s ! np.g ! np.n ++ copula ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ;
      Future = \\pol => comp.s ! np.g ! np.n ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      FutureInterrogative = \\pol => comp.s ! np.g ! np.n ++ futureAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "vera" ;
      Conditional = \\pol => comp.s ! np.g ! np.n ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom ;
      ConditionalInterrogative = \\pol => comp.s ! np.g ! np.n ++ conditionalAux ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "vera" ;
      Anterior = \\t,pol => comp.s ! np.g ! np.n ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "verið" ++ np.s ! Nom ;
      AnteriorInterrogative = \\t,pol => comp.s ! np.g ! np.n ++ perfectAux ! t ! persNum np.n np.p ++ np.s ! Nom ++ negStr pol ++ "verið" ;
      Nonfinite = comp.s ! np.g ! np.n ++ "vera" ++ np.s ! Nom ;
      Participle = \\_ => comp.s ! np.g ! np.n ++ "verið" ++ np.s ! Nom
    } ;

}
