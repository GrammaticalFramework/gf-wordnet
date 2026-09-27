concrete ParseExtendChi of ParseExtend =
  ExtendChi - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralChi - [num], PunctuationX **
  open Prelude, ResChi, GrammarChi, (G=GrammarChi), (E=ExtendChi) in {

lin gen_Quant = DefArt ;

lin UttAP pron ap = {s = ap.s ! Pred} ;
    UttVPS pron vps = {s = vps.s} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin FocusComp comp np = PredVP np (UseComp comp) ;

lincat CNN = {num1,num2 : Num ; cn1,cn2 : CN} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      num1 = num1 ; cn1 = cn1 ; num2 = num2 ; cn2 = cn2
      } ;
    DetCNN quant conj cnn =
      G.ConjNP conj (G.BaseNP
        (DetCN (DetQuant quant cnn.num1) cnn.cn1)
        (DetCN (DetQuant quant cnn.num2) cnn.cn2)) ;
    ReflPossCNN conj cnn =
      G.ConjNP conj (G.BaseNP
        (E.ReflPoss cnn.num1 cnn.cn1)
        (E.ReflPoss cnn.num2 cnn.cn2)) ;
    PossCNN_RNP quant conj cnn rnp =
      G.ConjNP conj (G.BaseNP
        (DetCN (DetQuant quant cnn.num1) (PossNP cnn.cn1 rnp))
        (DetCN (DetQuant quant cnn.num2) (PossNP cnn.cn2 rnp))) ;

lin NumMore n = n ** {s = n.s ++ "多"} ;
    NumLess n = n ** {s = n.s ++ "少"} ;
    UseACard card = card ;
    UseAdAACard ada card = {s = ada.s ++ card.s} ;

lin BareN2 n = {s = n.s ; c = n.c} ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ infVP comp ;
      advType = ATManner ; hasDe = False
      } ;
    CAdvAP pol cadv ap comp = ap ** {
      s = \\af => pol.s ++ cadv.s ++ ap.s ! af ++ cadv.p ++ infVP comp
      } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.p} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\af => "足够" ++ ap.s ! af ++ "能" ++
                  pol.s ++ useVerb vp.verb ! pol.p ! ant.t ++ vp.compl
      } ;
    EnoughAdv adv = adv ** {s = "足够" ++ adv.s} ;
    ExtAdvAP ap adv = ap ** {
      s = \\af => ap.s ! af ++ chcomma ++ adv.s
      } ;

lin TimeNP np = {
      s = linNP np ; advType = ATTime ; hasDe = False
      } ;
    AdvAdv adv1 adv2 = adv2 ** {s = adv1.s ++ adv2.s} ;

lin whatSgFem_IP, whatSgNeut_IP = whatSg_IP ;
    that_RP = IdRP ;

lin EmbedVP ant pol pron vp = {
      s = vp.topic ++ vp.prePart ++
          useVerb vp.verb ! pol.p ! ant.t ++ vp.compl
      } ;

lin ComplVV v ant pol vp = {
      verb = v ;
      compl = vp.topic ++ vp.prePart ++
              useVerb vp.verb ! pol.p ! ant.t ++ vp.compl ;
      prePart, topic = [] ;
      isAdj = False ;
      } ;

    SlashVV v ant pol slash = {
      verb = v ;
      compl = slash.topic ++ slash.prePart ++
              useVerb slash.verb ! pol.p ! ant.t ++ slash.compl ;
      prePart, topic = [] ; isAdj = False ;
      c2 = slash.c2 ; isPre = slash.isPre
      } ;

    SlashV2V v ant pol vp =
      insertObj (mkNP (vp.topic ++ vp.prePart ++
                       useVerb vp.verb ! pol.p ! ant.t ++ vp.compl))
        (predV v v.part) ** {c2 = v.c2 ; isPre = v.hasPrep} ;

    SlashV2VNP v np ant pol slash =
      insertObj np
        (insertObj (mkNP (slash.topic ++ slash.prePart ++
                          useVerb slash.verb ! pol.p ! ant.t ++ slash.compl))
          (predV v v.part)) ** {c2 = slash.c2 ; isPre = slash.isPre} ;

    InOrderToVP ant pol pron vp = {
      s = "为了" ++ vp.topic ++ vp.prePart ++
          useVerb vp.verb ! pol.p ! ant.t ++ vp.compl ;
      advType = ATTime ; hasDe = False
      } ;

    CompVP ant pol pron vp = {
      verb = noVerb ;
      compl = vp.topic ++ vp.prePart ++
              useVerb vp.verb ! pol.p ! ant.t ++ vp.compl ;
      prePart, topic = [] ; isAdj = False
      } ;

    UttVP ant pol pron vp = {
      s = vp.topic ++ vp.prePart ++
          useVerb vp.verb ! pol.p ! ant.t ++ vp.compl
      } ;

    ReflA2 a rnp = G.ComplA2 a rnp ;
    ReflVPSlash slash rnp = G.ComplSlash slash rnp ;

lin RecipVPSlash slash = insertAdv (ss "互相") <slash : ResChi.VP> ;
    RecipVPSlashCN slash cn = G.ComplSlash slash
      (mkNP ("彼此" ++ possessive_s ++ cn.s)) ;

lin num x = x ;

lin RelNP = GrammarChi.RelNP ;
    ExtRelNP = GrammarChi.RelNP ;

}
