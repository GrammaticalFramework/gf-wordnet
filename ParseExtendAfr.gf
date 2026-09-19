concrete ParseExtendAfr of ParseExtend =
  ExtendAfr - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
               GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
               UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralAfr - [num], PunctuationX **
  open Prelude, ResAfr, GrammarAfr, ExtendAfr in {

lin UttAP  p ap  = {s = ap.s ! APred} ;
    UttVPS p vps = {s = vps.s ! Main ! p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarAfr.RelNP ;
    ExtRelNP = GrammarAfr.RelNP ;

lin BareN2 n = n ;

lin that_RP = IdRP ;

lin gen_Quant = {
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
      s = \\a => reflPossDet a ++ conj.s1 ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ;
      isPron = False
      } ;
    PossCNN_RNP quant conj cnn rnp = {
      s = \\a => quant.s ! False ! cnn.n1 ! cnn.g ++
                   conj.s1 ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ++ "van" ++ rnp.s ! a ;
      isPron = False
      } ;

lin NumMore n = {s = n.s ++ "meer" ; n = Pl ; isNum = n.isNum} ;
    NumLess n = {s = n.s ++ "minder" ; n = Pl ; isNum = n.isNum} ;
    UseACard card = {s = \\_,_ => card.s ; n = Pl} ;
    UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s ; n = Pl} ;

lin ExtAdvAP ap adv = {
      s = \\af => ap.s ! af ++ "," ++ adv.s ;
      isPre = False
      } ;
    ComparAdv pol cadv adv comp = {
      s = negOnce pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Sg
      } ;
    CAdvAP pol cadv ap comp = {
      s = \\af => negOnce pol.p ++ cadv.s ++ ap.s ! af ++ cadv.p ++ comp.s ! agrP3 Sg ;
      isPre = False
      } ;
    AdnCAdv pol cadv = {s = negOnce pol.p ++ cadv.s ++ cadv.p} ;
    EnoughAP ap ant pol vp = {
      s = \\af => ap.s ! af ++ "genoeg" ++ infVPFull False vp (agrP3 Sg) ant.a pol.p ;
      isPre = False
      } ;
    EnoughAdv adv = {s = adv.s ++ "genoeg"} ;
    TimeNP np = {s = np.s ! NPAcc} ;
    AdvAdv a b = {s = a.s ++ b.s} ;
    whatSgFem_IP = whatSg_IP ;
    whatSgNeut_IP = whatSg_IP ;

lin EmbedVP ant pol pron vp = {s = infVPFull False vp pron.a ant.a pol.p} ;
    ComplVV vv ant pol vp =
      insertObj (\\a => infVPFull vv.isAux vp a ant.a pol.p) (predVGen vv.isAux vv) ;
    SlashVV vv ant pol slash =
      insertObj (\\a => infVPFull vv.isAux slash a ant.a pol.p)
        (predVGen vv.isAux vv) ** {c2 = slash.c2} ;
    SlashV2V v ant pol vp =
      insertObj (\\a => infVPFull v.isAux vp a ant.a pol.p)
        (predVGen v.isAux v) ** {c2 = v.c2} ;
    SlashV2VNP v np ant pol slash =
      insertObj (\\a => appPrep v.c2 np.s ++ infVPFull v.isAux slash a ant.a pol.p)
        (predVGen v.isAux v) ** {c2 = slash.c2} ;
    InOrderToVP ant pol pron vp = {
      s = "ten einde" ++ infVPFull False vp pron.a ant.a pol.p
      } ;
    CompVP ant pol pron vp = {
      s = \\_ => infVPFull False vp pron.a ant.a pol.p
      } ;
    UttVP ant pol pron vp = {s = infVPFull False vp pron.a ant.a pol.p} ;

    ReflA2 = ExtendAfr.ReflA2RNP ;
    ReflVPSlash = ExtendAfr.ReflRNP ;
    RecipVPSlash slash = insertObj (\\_ => appPrep slash.c2 (\\_ => "mekaar")) slash ;
    RecipVPSlashCN slash cn = insertObj
      (\\_ => appPrep slash.c2 (\\_ => "mekaar" ++ "se" ++ cn.s ! Strong ! NF Sg Nom)) slash ;

    FocusComp comp np = mkClause (comp.s ! np.a) np.a
      (insertObj (\\_ => np.s ! NPNom) (predV zijn_V)) ;

oper
  negOnce : Polarity -> Str = \p -> case p of {Pos => [] ; Neg => "nie"} ;

  infVPFull : Bool -> ResAfr.VP -> Agr -> Anteriority -> Polarity -> Str =
    \isAux,vp,a,ant,pol ->
      let neg = negOnce pol ;
          obj = vp.n0 ! a ++ vp.n2 ! a ++ vp.a2 ;
          tail = case pol of {Pos => [] ; Neg => "nie"} ;
          verb = case ant of {
            Simul => case isAux of {
              True => neg ++ obj ++ vp.s.prefix ++ BIND ++ vp.s.s ! VInf ++ vp.inf.p1 ;
              False => "om" ++ neg ++ obj ++ vp.s.prefix ++ "te" ++
                       vp.s.s ! VInf ++ vp.inf.p1
              } ;
            Anter => "om" ++ neg ++ obj ++ vp.s.s ! VPerf ++ "te" ++ "hê" ++ vp.inf.p1
            }
      in case ant of {
        Simul => verb ++ vp.ext ++ tail ;
        Anter => verb ++ vp.ext ++ tail
        } ;

}
