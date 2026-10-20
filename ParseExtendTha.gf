concrete ParseExtendTha of ParseExtend =
  ExtendTha - [
    iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
    GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
    CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP,
    UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
    PiedPipingQuestSlash, PiedPipingRelSlash
  ],
  NumeralTha - [num], PunctuationX **
  open Prelude, GrammarTha, ResTha in {

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarTha.RelNP ;
    ExtRelNP = GrammarTha.RelNP ;

lin that_RP = {s = "ที่"} ;

lin gen_Quant = DefArt ;

lin UttAP pron ap = {s = thbind pron.s ap.s} ;
    UttVPS pron vps = {s = thbind pron.s vps.s} ;

lincat CNN = {s1, s2 : Str} ;

lin BaseCNN n1 cn1 n2 cn2 = {s1 = cn1.s ; s2 = cn2.s} ;
    DetCNN quant conj cnn = {
      s = thbind cnn.s1 conj.s2 cnn.s2 quant.s1 quant.s2
      } ;
    ReflPossCNN conj cnn = {
      s = thbind cnn.s1 conj.s2 cnn.s2 "ของ" "ตัวเอง"
      } ;
    PossCNN_RNP quant conj cnn rnp = {
      s = thbind (thbind cnn.s1 conj.s2 cnn.s2 quant.s1 quant.s2 conj.s2) rnp.s
      } ;

lin NumLess n = n ** {s = thbind n.s "น้อยกว่า"} ;
    NumMore n = n ** {s = thbind n.s "มากกว่า"} ;
    UseACard n = n ;
    UseAdAACard ada n = {s = thbind ada.s n.s} ;

lin BareN2 n = {s = n.s ; c = n.c} ;
    TimeNP np = {s = np.s} ;
    AdvAdv a b = {s = thbind a.s b.s} ;
    ExtAdvAP ap adv = {s = thbind ap.s adv.s} ;

lin ComparAdv pol cadv adv comp = {
      s = thbind pol.s adv.s cadv.s cadv.p (comp.s ! pol.p)
      } ;
    CAdvAP pol cadv ap comp = {
      s = thbind pol.s ap.s cadv.s cadv.p (comp.s ! pol.p)
      } ;
    AdnCAdv pol cadv = {s = thbind pol.s cadv.s} ;

lin EnoughAP ap ant pol vp = {s = thbind ap.s "พอ" (vp.s ! pol.p) vp.e} ;
    EnoughAdv adv = {s = thbind adv.s "พอ"} ;

lin ComplVV vv ant pol vp = GrammarTha.ComplVV vv (fixedVP ant pol vp) ;
    SlashVV vv ant pol vp = GrammarTha.SlashVV vv (fixedVPSlash ant pol vp) ;
    SlashV2V v ant pol vp = GrammarTha.SlashV2V v (fixedVP ant pol vp) ;
    SlashV2VNP v np ant pol vp =
      GrammarTha.SlashV2VNP v np (fixedVPSlash ant pol vp) ;
    EmbedVP ant pol pron vp = {s = thbind ant.s (vp.s ! pol.p) vp.e} ;
    InOrderToVP ant pol pron vp = {s = thbind "เพื่อ" ant.s (vp.s ! pol.p) vp.e} ;
    CompVP ant pol pron vp = {s = \\_ => thbind ant.s (vp.s ! pol.p) vp.e} ;
    UttVP ant pol pron vp = {s = thbind pron.s ant.s (vp.s ! pol.p) vp.e} ;

lin ReflVPSlash vp rnp = GrammarTha.ComplSlash vp rnp ;
    ReflA2 a rnp = GrammarTha.ComplA2 a rnp ;
    RecipVPSlash vp = GrammarTha.ComplSlash vp (lin NP {s = "กันและกัน"}) ;
    RecipVPSlashCN vp cn =
      GrammarTha.ComplSlash vp (lin NP {s = thbind cn.s "ของกันและกัน"}) ;

lin FocusComp comp np = GrammarTha.PredVP np (GrammarTha.UseComp comp) ;

lin whatSgFem_IP = GrammarTha.whatSg_IP ;
    whatSgNeut_IP = GrammarTha.whatSg_IP ;

oper fixedVP : Ant -> Pol -> VP -> VP = \ant,pol,vp -> lin VP {
       s = \\_ => thbind ant.s (vp.s ! pol.p) ;
       e = vp.e
       } ;

     fixedVPSlash : Ant -> Pol -> VPSlash -> VPSlash = \ant,pol,vp ->
       lin VPSlash (fixedVP ant pol vp ** {c2 = vp.c2}) ;

}
