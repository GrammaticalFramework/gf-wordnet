concrete ParseExtendMlt of ParseExtend =
 CatMlt, ExtendMlt - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
                      AdvIsNPAP, DetNPFem, DetNPMasc, ExistCN,
                      PiedPipingQuestSlash, PiedPipingRelSlash,
                      ReflA2RNP,
                      ReflPossPron, ApposNP,
                      CompVP, ProgrVPSlash, UseDAP, UseDAPMasc, UseDAPFem],
 NumeralMlt - [num], PunctuationX ** open Prelude, Maybe, ResMlt, GrammarMlt in {

oper
  useDAP : Gender -> DAP -> NP = \g,dap -> lin NP {
    s = \\_ => dap.s ! g ++ dap.adj ! mkGenNum g (numform2num dap.n) ;
    a = agrP3 (numform2num dap.n) g ;
    isPron = False ;
    isDefn = dap.isDefn
    } ;

  cnWithNum : Num -> CN -> Str = \num,cn ->
    num.s ! NumAdj ++ cn.s ! numform2nounnum num.n ;

lincat
  CNN = {s1,s2 : Str ; n1,n : Number ; g1 : Gender} ;

lin gen_Quant = DefArt ;

lin UttAP p ap = { s = ap.s ! mkGenNum p.a.g p.a.n } ;
    UttVPS p vps = {s = vps.s ! p.a} ;
    UttVP ant pol p vp = {s = ant.s ++ pol.s ++ infVP vp ant.a pol.p p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarMlt.RelNP ;
    ExtRelNP = GrammarMlt.RelNP ;

lin BareN2 n = n ;

lin EmbedVP ant pol p vp = {s = ant.s ++ pol.s ++ infVP vp ant.a pol.p p.a} ;
    ComplVV vv ant pol vp =
      insertObj (\\agr => ant.s ++ pol.s ++ infVP vp ant.a pol.p agr) (predV vv) ;
    SlashVV vv ant pol vp =
      insertObj (\\agr => ant.s ++ pol.s ++ infVP vp ant.a pol.p agr) (predV vv) ** {c2 = vp.c2} ;
    SlashV2V v ant pol vp =
      insertObjc (\\agr => v.c3.s ! Definite ++ ant.s ++ pol.s ++ infVP vp ant.a pol.p agr) (predVc v) ;
    SlashV2VNP vv np ant pol vp =
      insertObjPre (\\_ => vv.c2.s ! Definite ++ np.s ! NPAcc)
        (insertObjc (\\agr => vv.c3.s ! Definite ++ ant.s ++ pol.s ++ infVP vp ant.a pol.p agr) (predVc vv)) **
          {c2 = vp.c2} ;
    CompVP ant pol p vp = {s = \\_ => "li" ++ ant.s ++ pol.s ++ infVP vp ant.a pol.p p.a} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = cnWithNum num1 cn1 ;
      s2 = cnWithNum num2 cn2 ;
      n1 = numform2num num1.n ;
      n = conjNumber (numform2num num1.n) (numform2num num2.n) ;
      g1 = cn1.g
      } ;
    DetCNN quant conj cnn = {
      s = \\_ => quant.s ! mkGenNum cnn.g1 cnn.n1 ++ conj.s1 ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ;
      a = agrP3 cnn.n cnn.g1 ;
      isPron = False ;
      isDefn = quant.isDefn
      } ;
    ReflPossCNN conj cnn = {
      s = \\agr => conj.s1 ++ cnn.s1 ++ prep_ta.enclitic ! agr ++ conj.s2 ++ cnn.s2 ++ prep_ta.enclitic ! agr
      } ;
    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr => quant.s ! mkGenNum cnn.g1 cnn.n1 ++ conj.s1 ++ cnn.s1 ++ prep_ta.s ! Definite ++ rnp.s ! agr ++ conj.s2 ++ cnn.s2 ++ prep_ta.s ! Definite ++ rnp.s ! agr
      } ;

lin NumLess num = num ** {
      s = \\c => num.s ! c ++ "inqas" ;
      n = Num20_99
      } ;
    NumMore num = num ** {
      s = \\c => num.s ! c ++ "iktar" ;
      n = Num20_99
      } ;
    UseACard card = card ;
    UseAdAACard ada card = {
      s = \\c => ada.s ++ card.s ! c ;
      n = card.n
      } ;

lin UseDAP dap = useDAP Masc dap ;
    UseDAPMasc dap = useDAP Masc dap ;
    UseDAPFem dap = useDAP Fem dap ;

lin ComparAdv pol cadv adv comp = advSS (pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Sg Masc) ;
    CAdvAP pol cadv ap comp = {
      s = \\gn => pol.s ++ cadv.s ++ ap.s ! gn ++ cadv.p ++ comp.s ! toAgr gn ;
      isPre = False
      } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.p} ;
    EnoughAP ap ant pol vp = {
      s = \\gn => ap.s ! gn ++ "biżżejjed" ++ "biex" ++ infVP vp ant.a pol.p (toAgr gn) ;
      isPre = False
      } ;
    EnoughAdv adv = advSS (adv.s ++ "biżżejjed") ;
    ExtAdvAP ap adv = {
      s = \\gn => ap.s ! gn ++ "," ++ adv.s ;
      isPre = False
      } ;
    AdvAdv adv1 adv2 = advSS (adv1.s ++ adv2.s) ;
    TimeNP np = advSS (np.s ! NPNom) ;
    ApposNP np appos = {
      s = \\c => np.s ! c ++ "," ++ appos.s ! c ;
      a = np.a ;
      isPron = False ;
      isDefn = np.isDefn
      } ;
    FocusComp comp np =
      mkClause (comp.s ! np.a) np.a (insertObj (\\_ => np.s ! NPAcc) CopulaVP) ;

lin whatSgFem_IP = GrammarMlt.whatSg_IP ;
    whatSgNeut_IP = GrammarMlt.whatSg_IP ;
    that_RP = IdRP ;

lin RecipVPSlash vp =
      ComplSlash vp {
        s = \\_ => "xulxin" ;
        a = agrP3 Pl Masc ;
        isPron = False ;
        isDefn = True
        } ;
    RecipVPSlashCN vp cn =
      ComplSlash vp {
        s = \\_ => cn.s ! Plural ++ prep_ta.s ! Definite ++ "xulxin" ;
        a = agrP3 Pl cn.g ;
        isPron = False ;
        isDefn = True
        } ;

    InOrderToVP ant pol p vp = advSS ("biex" ++ infVP vp ant.a pol.p p.a) ;

    ReflVPSlash vp rnp = insertObj (\\agr => complRNP vp.c2 rnp agr) vp ;
    ReflA2 a2 rnp = {
      s = \\gn => a2.s ! APosit gn ++ complRNP a2.c2 rnp (toAgr gn) ;
      isPre = False
      } ;

}
