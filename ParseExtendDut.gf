concrete ParseExtendDut of ParseExtend =
  ExtendDut - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralDut - [num], PunctuationX ** 
  open Prelude, ResDut, GrammarDut, Coordination, ExtendDut in {

lin UttAP  p ap  = {s = ap.s ! p.a ! APred} ;
    UttVPS p vps = {s = vps.s ! Main ! p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin that_RP = IdRP ;

lin RelNP = GrammarDut.RelNP ;
    ExtRelNP = GrammarDut.RelNP ;

lin BareN2 n = n ;

lin gen_Quant = noMerge ** {
      s = \\_,_,_ => [] ;
      sp = \\_,_ => [] ;
      a = Strong
      } ;

lincat CNN = {s1,s2 : Str ; n1,n : Number ; g : Gender} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = num1.s ++ cn1.s ! Strong ! NF num1.n Nom ;
      s2 = num2.s ++ cn2.s ! Strong ! NF num2.n Nom ;
      n1 = num1.n ;
      n = conjNumber num1.n num2.n ;
      g = cn1.g
      } ;

    DetCNN quant conj cnn = heavyNP {
      s = \\_ => quant.s ! False ! cnn.n1 ! cnn.g ++
                   conj.s1 ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ;
      a = agrP3 (conjNumber conj.n cnn.n)
      } ;

    ReflPossCNN conj cnn = {
      s = \\agr => reflPossDet agr cnn.n1 cnn.g ++
                    conj.s1 ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ;
      isPron = False
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr => quant.s ! False ! cnn.n1 ! cnn.g ++
                    conj.s1 ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ++
                    "van" ++ rnp.s ! agr ;
      isPron = False
      } ;

lin NumMore num = {s = num.s ++ "meer" ; n = Pl ; isNum = num.isNum} ;
    NumLess num = {s = num.s ++ "minder" ; n = Pl ; isNum = num.isNum} ;

    UseACard card = {s = \\_,_ => card.s ; n = Pl} ;
    UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s ; n = Pl} ;

lin ExtAdvAP ap adv = {
      s = \\agr,af => ap.s ! agr ! af ++ bindComma ++ adv.s ;
      isPre = False
      } ;

    ComparAdv pol cadv adv comp = {
      s = pol.s ++ negInf pol.p ++ cadv.s ++ adv.s ++ cadv.p ++
          comp.s ! agrP3 Sg
      } ;

    CAdvAP pol cadv ap comp = {
      s = \\agr,af => pol.s ++ negInf pol.p ++ cadv.s ++
                       ap.s ! agr ! af ++ cadv.p ++ comp.s ! agr ;
      isPre = False
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ negInf pol.p ++ cadv.s ++ cadv.p} ;

    EnoughAP ap ant pol vp = {
      s = \\agr,af => ap.s ! agr ! af ++ "genoeg" ++ "om" ++
                       infVPFull False vp agr ant.a pol.p ;
      isPre = False
      } ;

    EnoughAdv adv = {s = adv.s ++ "genoeg"} ;
    TimeNP np = {s = np.s ! NPAcc} ;
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

    whatSgFem_IP, whatSgNeut_IP = whatSg_IP ;

lin EmbedVP ant pol p vp = {s = infVPFull False vp p.a ant.a pol.p} ;

    ComplVV vv ant pol vp = case <ant.a,pol.p> of {
      <Simul,Pos> => GrammarDut.ComplVV vv vp ;
      _ => insertObj (\\agr => infVPFull vv.isAux vp agr ant.a pol.p)
                     (predVGen vv.isAux vp.negPos vv)
      } ;

    SlashVV vv ant pol slash = case <ant.a,pol.p> of {
      <Simul,Pos> => GrammarDut.SlashVV vv slash ;
      _ => insertObj (\\agr => infVPFull vv.isAux slash agr ant.a pol.p)
                     (predVGen vv.isAux slash.negPos vv) ** {c2 = slash.c2}
      } ;

    SlashV2V verb ant pol vp = case <ant.a,pol.p> of {
      <Simul,Pos> => GrammarDut.SlashV2V verb vp ;
      _ => insertObj (\\agr => infVPFull verb.isAux vp agr ant.a pol.p)
                     (predVGen verb.isAux vp.negPos verb) ** {c2 = verb.c2}
      } ;

    SlashV2VNP verb np ant pol slash = case <ant.a,pol.p> of {
      <Simul,Pos> => GrammarDut.SlashV2VNP verb np slash ;
      _ => insertObj
             (\\agr => appPrep verb.c2.p1 np ++
                       infVPFull verb.isAux slash agr ant.a pol.p)
             (predVGen verb.isAux slash.negPos verb) ** {c2 = slash.c2}
      } ;

    InOrderToVP ant pol p vp = {
      s = "om" ++ infVPFull False vp p.a ant.a pol.p
      } ;

    CompVP ant pol p vp = {
      s = \\_ => infVPFull False vp p.a ant.a pol.p
      } ;

    UttVP ant pol p vp = {s = infVPFull False vp p.a ant.a pol.p} ;

    ReflA2 = ExtendDut.ReflA2RNP ;
    ReflVPSlash = ExtendDut.ReflRNP ;

    RecipVPSlash slash = GrammarDut.ComplSlash slash (recipNP "elkaar") ;
    RecipVPSlashCN slash cn = GrammarDut.ComplSlash slash
      (recipNP ("elkaars" ++ cn.s ! Strong ! NF Sg Nom)) ;

    FocusComp comp np = mkClause (comp.s ! np.a) np.a
      (insertObj (\\_ => np.s ! NPNom) (predV zijn_V)) ;

oper
  negInf : Polarity -> Str = \pol -> case pol of {Pos => [] ; Neg => "niet"} ;

  infVPFull : Bool -> ResDut.VP -> Agr -> Anteriority -> Polarity -> Str =
    \isAux,vp,agr,ant,pol ->
      let
        obj = vp.n0 ! agr ++ vp.n2 ! agr ++ vp.a2 ;
        neg = vp.a1 ! pol ;
        simul = case isAux of {
          True => vp.s.s ! VInfFull ;
          False => vp.s.prefix ++ "te" ++ vp.s.s ! VInf
          } ;
        anter = vp.s.s ! VPerf APred ++ "te" ++ (auxVerb vp.s.aux).s ! VInf
      in neg ++ obj ++ vp.s.particle ++
         case ant of {Simul => simul ; Anter => anter} ++
         vp.inf.p1 ++ vp.ext ;

  recipNP : Str -> NounPhrase = \s -> heavyNP {
    s = \\_ => s ;
    a = agrP3 Sg
    } ;

}
