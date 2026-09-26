concrete ParseExtendEus of ParseExtend =
  ExtendEus - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralEus - [num], PunctuationX **
  open Prelude, ResEus, (G=GrammarEus) in {

lincat CNN = {num1,num2 : Num ; cn1,cn2 : CN} ;

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin UttAP _ ap = { s = ap.s ! Hau } ;

lin UttVPS pron vps = {s = vps.s ! pron.agr} ;

lin gen_Quant = G.DefArt ;

lin ReflA2 a2 rnp = G.ComplA2 a2 rnp ;
    ReflVPSlash vps rnp = G.ComplSlash vps rnp ;

lin BaseCNN num1 cn1 num2 cn2 = {
      num1 = num1 ; cn1 = cn1 ; num2 = num2 ; cn2 = cn2
      } ;
    DetCNN quant conj cnn =
      G.ConjNP conj (G.BaseNP
        (G.DetCN (G.DetQuant quant cnn.num1) cnn.cn1)
        (G.DetCN (G.DetQuant quant cnn.num2) cnn.cn2)) ;
    ReflPossCNN conj cnn =
      G.ConjNP conj (G.BaseNP
        (ReflPoss cnn.num1 cnn.cn1)
        (ReflPoss cnn.num2 cnn.cn2)) ;
    PossCNN_RNP quant conj cnn rnp =
      G.ConjNP conj (G.BaseNP
        (G.DetCN (G.DetQuant quant cnn.num1) (G.PossNP cnn.cn1 rnp))
        (G.DetCN (G.DetQuant quant cnn.num2) (G.PossNP cnn.cn2 rnp))) ;

lin NumLess n = n ** {s = n.s ++ "gutxiago" ; isNum = True} ;
    NumMore n = n ** {s = n.s ++ "gehiago" ; isNum = True} ;
    UseACard card = {s = card.s ; n = Pl} ;
    UseAdAACard ada card = {s = ada.s ++ card.s ; n = Pl} ;

lin RelNP np rs = np ** {s = \\c => rs.s ! np.agr ++ np.s ! c} ;
    ExtRelNP = G.RelNP ;
    ExtAdvAP ap adv = ap ** {s = \\agr => ap.s ! agr ++ SOFT_BIND ++ "," ++ adv.s} ;
    BareN2 n = n ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! Hau
      } ;
    CAdvAP pol cadv ap comp = ap ** {
      s = \\agr => pol.s ++ cadv.s ++ ap.s ! agr ++ cadv.p ++ comp.s ! agr
      } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.p} ;
    EnoughAP ap ant pol vp = ap ** {
      s = \\agr => ap.s ! agr ++ "bezain" ++ infinitiveStr pol vp agr
      } ;
    EnoughAdv adv = {s = adv.s ++ "nahikoa"} ;
    TimeNP np = {s = np.s ! Ine} ;
    AdvAdv a b = {s = a.s ++ b.s} ;

lin whatSgFem_IP = G.whatSg_IP ;
    whatSgNeut_IP = G.whatSg_IP ;

lin that_RP = G.IdRP ;

lin EmbedVP ant pol pron vp = {s = infinitiveStr pol vp pron.agr} ;
    ComplVV vv ant pol vp =
      insertComp (\\agr => infinitiveStr pol vp agr) (useV vv) ;
    SlashVV vv ant pol vps =
      (insertComp (\\agr => infinitiveStr pol vps agr) (useV vv)) ** {
        post = vps.post ; missing = vps.missing
        } ;
    SlashV2V v ant pol vp = (slashDObj v) ** {
      comp = \\agr => infinitiveStr pol vp agr
      } ;
    SlashV2VNP v np ant pol vps = (slashDObj v) ** {
      iobj = {s = np.s ! Dat ; agr = np.agr} ;
      comp = \\agr => infinitiveStr pol vps agr ;
      post = vps.post ; missing = vps.missing
      } ;
    InOrderToVP ant pol pron vp = {
      s = infinitiveStr pol vp pron.agr ++ "asmoz"
      } ;
    CompVP ant pol pron vp = {
      s = \\agr => infinitiveStr pol vp pron.agr ; copula = Izan
      } ;
    UttVP ant pol pron vp = {s = infinitiveStr pol vp pron.agr} ;

lin RecipVPSlash vps = G.ComplSlash vps reciprocalNP ;
    RecipVPSlashCN vps cn =
      G.ComplSlash vps (G.DetCN (G.DetQuant G.DefArt G.NumPl)
        (G.PossNP cn reciprocalNP)) ;
    FocusComp comp np = G.PredVP np (G.UseComp comp) ;

lin num x = x ;

oper
  infinitiveStr : Pol -> VerbPhrase -> Agr -> Str = \pol,vp,agr ->
    case pol.p of {
      Pos => linVPPrc vp ! agr ;
      Neg => vp.adv ++ vp.iobj.s ++ vp.dobj.s ! Neg ++ vp.comp ! agr
          ++ "ez" ++ vp.prc ! Past
      } ;

  reciprocalNP : NounPhrase = {
    s = table {
      Abs => "elkar" ; Erg => "elkarrek" ; Dat => "elkarri" ;
      Gen => "elkarren" ; Soc => "elkarrekin" ; Ins => "elkarrez" ;
      Ine => "elkarrengan" ; LocStem => "elkarrenga" ; Par => "elkarrik"
      } ;
    stem = "elkar" ; agr = Hauek ; anim = Anim ; isDef = True
    } ;

}
