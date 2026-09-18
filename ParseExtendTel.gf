--# -path=.:../gf-rgl/src/telugu:../gf-rgl/src/abstract:../gf-rgl/src/common:../gf-rgl/src/prelude

concrete ParseExtendTel of ParseExtend =
  ExtendTel - [
    iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
    GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
    CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
    UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
    PiedPipingQuestSlash, PiedPipingRelSlash
  ],
  NumeralTel - [num],
  PunctuationX ** open Prelude, ResTel in {

  lincat CNN = {s : Str ; n : Number ; g : Gender} ;

  lin
    PhrUttMark pconj utt voc mark =
      {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    num x = {s = x.s ; n = x.n} ;

    UseACard card = {s = card.s ; n = card.n} ;
    UseAdAACard ada card = {s = ada.s ++ card.s ; n = card.n} ;
    NumLess number = number ** {s = number.s ++ "తక్కువ"} ;
    NumMore number = number ** {s = number.s ++ "ఎక్కువ"} ;

    TimeNP np = {s = np.s ! NPC Obl} ;

    RelNP np rs = {
      s = \\c => rs.s ++ np.s ! c ;
      a = np.a
      } ;

    ExtRelNP np rs = {
      s = \\c => np.s ! c ++ rs.s ;
      a = np.a
      } ;

    InOrderToVP ant pol pron vp = {
      s = let f = vp.s ! pol.p ! VPInf in
        pron.s ! PC Dir ++ vp.obj.s ++ vp.comp ! pron.a ++ f.neg ++ f.inf ++ f.fin ++ "కోసం"
      } ;

    that_RP = {s = "అది"} ;
    whatSgFem_IP = {s = \\_ => "ఏది" ; n = Sg} ;
    whatSgNeut_IP = {s = \\_ => "ఏమిటి" ; n = Sg} ;

    gen_Quant = {s = \\_,_,_ => []} ;

    UttAP pron ap = {s = pron.s ! PC Dir ++ case pron.a of {
      Ag g n _ => ap.s ! g ! n ! Dir
      }} ;

    UttVPS pron vps = {s = pron.s ! PC Dir ++ vps.s ! pron.a} ;

    ReflVPSlash vps rnp = insertObject rnp vps ;

    ReflA2 adjective rnp = {
      s = \\g,n,c => rnp.s ! NPC Obl ++ adjective.c2 ++
                       adjective.s ! g ! n ! c
      } ;

    BaseCNN n1 cn1 n2 cn2 = {
      s = cn1.s ! n1.n ! Dir ++ cn2.s ! n2.n ! Dir ;
      n = Pl ;
      g = cn2.g
      } ;

    DetCNN quant conj cnn = {
      s = \\c => quant.s ! cnn.n ! cnn.g ! npcase2case c ++ cnn.s ;
      a = agrP3 cnn.g cnn.n
      } ;

    ReflPossCNN conj cnn = {
      s = \\_ => cnn.s ;
      a = agrP3 cnn.g cnn.n ;
      lock_NP = <>
      } ;

    AdvAdv first second = {s = first.s ++ second.s} ;

    ExtAdvAP ap adv = {
      s = \\g,n,c => ap.s ! g ! n ! c ++ "," ++ adv.s
      } ;

    BareN2 noun = {s = noun.s ; g = noun.g} ;

    ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! defaultAgr
      } ;

    CAdvAP pol cadv ap comp = {
      s = \\g,n,c => pol.s ++ cadv.s ++ ap.s ! g ! n ! c ++
                       cadv.p ++ comp.s ! Ag g n P3
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ cadv.s} ;

    EnoughAP ap ant pol vp = {
      s = \\g,n,c => let f = vp.s ! pol.p ! VPInf in
        ap.s ! g ! n ! c ++ "తగినంతగా" ++
        vp.obj.s ++ vp.comp ! Ag g n P3 ++ f.neg ++ f.inf ++ f.fin
      } ;

    EnoughAdv adv = {s = adv.s ++ "తగినంతగా"} ;

    ComplVV vv ant pol vp = predV vv ** {
      comp = \\agr => let f = vp.s ! pol.p ! VPInf in
        vp.obj.s ++ vp.comp ! agr ++ f.neg ++ f.inf ++ f.fin
      } ;

    SlashVV vv ant pol vps = predV vv ** {
      c2 = vps.c2 ;
      comp = \\agr => let f = vps.s ! pol.p ! VPInf in
        vps.obj.s ++ vps.comp ! agr ++ f.neg ++ f.inf ++ f.fin
      } ;

    EmbedVP ant pol pron vp = {
      s = let f = vp.s ! pol.p ! VPInf in
        pron.s ! PC Dir ++ vp.obj.s ++ vp.comp ! pron.a ++ f.neg ++ f.inf ++ f.fin
      } ;

    SlashV2V verb ant pol vp = predV verb ** {
      c2 = verb.c2 ;
      comp = \\agr => let f = vp.s ! pol.p ! VPInf in
        vp.obj.s ++ vp.comp ! agr ++ f.neg ++ f.inf ++ f.fin
      } ;

    SlashV2VNP verb np ant pol vps =
      let outer = insertObject np (predV verb ** {c2 = verb.c2})
      in outer ** {
        c2 = vps.c2 ;
        comp = \\agr => let f = vps.s ! pol.p ! VPInf in
          outer.comp ! agr ++ vps.obj.s ++ vps.comp ! agr ++ f.neg ++ f.inf ++ f.fin
        } ;

    CompVP ant pol pron vp = {
      s = \\agr => let f = vp.s ! pol.p ! VPInf in
        pron.s ! PC Dir ++ vp.obj.s ++ vp.comp ! agr ++ f.neg ++ f.inf ++ f.fin
      } ;

    UttVP ant pol pron vp = {
      s = let f = vp.s ! pol.p ! VPInf in
        pron.s ! PC Dir ++ vp.obj.s ++ vp.comp ! pron.a ++ f.neg ++ f.inf ++ f.fin
      } ;

    RecipVPSlash vps = insertObject reciprocalNP vps ;
    RecipVPSlashCN vps cn = insertObject
      {s = \\c => "ఒకరి" ++ cn.s ! Sg ! npcase2case c ; a = defaultAgr}
      vps ;

    FocusComp comp np = mkClause np (predCopula ** {comp = comp.s}) ;

    EmptyRelSlash slash = {
      s = \\t,p => slash.s ! t ! p ++ slash.c2.s
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\c => quant.s ! cnn.n ! cnn.g ! npcase2case c ++
                   cnn.s ++ conj.s2 ++ rnp.s ! c ;
      a = agrP3 cnn.g Pl ;
      lock_NP = <>
      } ;

  oper
    reciprocalNP : ResTel.NP = {
      s = \\c => case c of {
        NPC Dir => "ఒకరినొకరు" ;
        _ => "ఒకరినొకరి"
        } ;
      a = Ag Masc Pl P3
      } ;
}
