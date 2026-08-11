concrete ParseExtendSom of ParseExtend =
  CatSom,
  ExtendSom - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralSom - [num], PunctuationX ** open Prelude, ResSom, GrammarSom, (G=GrammarSom), ParadigmsSom in {

lin gen_Quant = DefArt ;

lin UttAP  p ap  = { s = ap.s ! AF (getNum p.a) Abs } ;
    UttVPS p vps = {s = vps.s ! p.a ! False} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin NumMore num = num ** {
      s = \\df => num.s ! df ++ "kale" ;
      n = Pl
      } ;

    NumLess num = num ** {
      s = \\df => num.s ! df ++ "ka yar" ;
      n = Pl
      } ;

lin UseACard card = baseNum ** {
      s = \\_ => card.s ;
      da = M KA ;
      n = Pl
      } ;

    UseAdAACard ada card = baseNum ** {
      s = \\_ => ada.s ++ card.s ;
      da = M KA ;
      n = Pl
      } ;

lin BareN2 n = n ;

lin FocusComp comp np = PredVP np (UseComp comp) ;

lincat CNN = {num1,num2 : Num ; cn1,cn2 : CN} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      num1 = num1 ;
      cn1 = cn1 ;
      num2 = num2 ;
      cn2 = cn2
      } ;

    DetCNN quant conj cnn =
      G.ConjNP conj (G.BaseNP
        (DetCN (DetQuant quant cnn.num1) cnn.cn1)
        (DetCN (DetQuant quant cnn.num2) cnn.cn2)) ;

    ReflPossCNN conj cnn =
      G.ConjNP conj (G.BaseNP
        (ReflPoss cnn.num1 cnn.cn1)
        (ReflPoss cnn.num2 cnn.cn2)) ;

    PossCNN_RNP quant conj cnn rnp =
      G.ConjNP conj (G.BaseNP
        (DetCN (DetQuant quant cnn.num1) (PossNP cnn.cn1 rnp))
        (DetCN (DetQuant quant cnn.num2) (PossNP cnn.cn2 rnp))) ;

lin RelNP = GrammarSom.RelNP ;
    ExtRelNP = GrammarSom.RelNP ;

lin ComparAdv pol cadv adv comp = adv ** {
      berri = pol.s ++ cadv.s ++ adv.berri ++ cadv.p ++ comp.aComp ! Sg3 Masc
      } ;

    CAdvAP pol cadv ap comp = ap ** {
      s = \\af => pol.s ++ cadv.s ++ ap.s ! af ++ cadv.p ++ comp.aComp ! Sg3 Masc
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.p} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\af => infVP vp ++ ap.s ! af
      } ;

    EnoughAdv adv = adv ** {
      berri = adv.berri ++ "ku filan"
      } ;

    ExtAdvAP ap adv = ap ** {
      s = \\af => ap.s ! af ++ "," ++ linAdv adv
      } ;

lin TimeNP np = mkAdv (np.s ! Abs) ;

lin AdvAdv adv1 adv2 = adv2 ** {
      berri = linAdv adv1 ++ adv2.berri
      } ;

lin whatSgFem_IP, whatSgNeut_IP = whatSg_IP ;

lin that_RP = IdRP ;

lin EmbedVP ant pol p vp = {s = infVP vp} ;

    ComplVV vv ant pol vp = GrammarSom.ComplVV vv vp ;

    SlashVV vv ant pol vps = GrammarSom.SlashVV vv vps ;

    SlashV2V v2v ant pol vp = GrammarSom.SlashV2V v2v vp ;

    SlashV2VNP v2v np ant pol vps = GrammarSom.SlashV2VNP v2v np vps ;

    InOrderToVP ant pol p vp = mkAdv ("si" ++ infVP vp) ;

    CompVP ant pol p vp = (CompAdv (mkAdv (infVP vp))) ;

    UttVP ant pol p vp = {s = infVP vp} ;

    ReflA2 a2 rnp = ComplA2 a2 rnp ;
    ReflVPSlash vps rnp = ComplSlash vps rnp ;

lin RecipVPSlash vps = GrammarSom.ReflVP vps ;
    RecipVPSlashCN vps cn = GrammarSom.ReflVP vps ;

}
