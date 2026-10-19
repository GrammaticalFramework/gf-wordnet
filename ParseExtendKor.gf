concrete ParseExtendKor of ParseExtend =
  CatKor, ExtendKor,
  NumeralKor - [num], PunctuationX ** open Prelude, ResKor, (G=GrammarKor) in {

lincat
  CNN = {num1,num2 : ResKor.Num ; cn1,cn2 : ResKor.CNoun} ;

lin UttAP p ap = {s = ap.compar ++ ap.s ! VF Formal Pos} ;

    gen_Quant = mkQuant [] [] ;

    UttVPS p vps = {s = vps.s} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin BareN2 n = n ;

lin BaseCNN n1 cn1 n2 cn2 = {num1=n1; cn1=cn1; num2=n2; cn2=cn2} ;
    DetCNN q co cnn = G.ConjNP co (G.BaseNP
      (G.DetCN (G.DetQuant q cnn.num1) cnn.cn1)
      (G.DetCN (G.DetQuant q cnn.num2) cnn.cn2)) ;
    ReflPossCNN co cnn = {s = "자신의" ++ cnn.cn1.s ! Bare ++
      co.s2 ++ "자신의" ++ cnn.cn2.s ! Bare} ;
    PossCNN_RNP q co cnn rnp = {s = cnn.cn1.s ! Bare ++ co.s2 ++
      cnn.cn2.s ! Bare ++ rnp.s} ;

    NumMore n = n ** {s = \\o,f => "더" ++ n.s ! o ! f} ;
    NumLess n = n ** {s = \\o,f => "덜" ++ n.s ! o ! f} ;
    UseACard c = {s=\\_,_=>c.s; n=Pl; numtype=NoNum} ;
    UseAdAACard a c = {s=\\_,_=>a.s ++ c.s; n=Pl; numtype=NoNum} ;

    RelNP np rs = np ** {s = \\nf => rs.s ! Subord ++ np.s ! nf} ;
    ExtRelNP np rs = np ** {s = \\nf => rs.s ! Subord ++ SOFT_BIND ++ "," ++ np.s ! nf} ;
    ExtAdvAP ap adv = ap ** {compar = adv.s ++ ap.compar} ;
    TimeNP np = {s = np.s ! Bare} ;
    AdvAdv a b = {s = a.s ++ b.s} ;
    EnoughAdv a = {s = "충분히" ++ a.s} ;
    EnoughAP ap ant pol vp = ap ** {
      compar = linVP (VAttr pol.p) vp ++ "만큼" ++ ap.compar
      } ;
    ComparAdv pol cadv adv comp = {s=pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! VAttr Pos} ;
    CAdvAP pol cadv ap comp = ap ** {
      compar=pol.s ++ cadv.s ++ ap.compar ++ cadv.p ++ comp.s ! VAttr Pos
      } ;
    AdnCAdv pol cadv = {s=pol.s ++ cadv.s ++ cadv.p} ;

    EmbedVP ant pol pron vp = {s = linVP (VAttr pol.p) vp ++ "것이"} ;
    ComplVV vv ant pol vp = useV vv ** {
      adv = vp.nObj ! DeclObj ++ vp.adv ++ vp.s ! VAttr pol.p ++ "것을"
      } ;
    SlashVV vv ant pol vps = vps ** {
      s = \\vf => vps.s ! VAttr pol.p ++ "것을" ++ vv.s ! vf
      } ;
    SlashV2V v ant pol vp = useVc v ** {
      adv = vp.nObj ! DeclObj ++ vp.adv ++ vp.s ! VAttr pol.p ++ "것을"
      } ;
    SlashV2VNP v np ant pol vps = vps ** {
      adv = (prepNP v.c2 v.p2 np).s ++ vps.adv ;
      s = \\vf => vps.s ! VAttr pol.p ++ "것을" ++ v.s ! vf
      } ;
    InOrderToVP ant pol pron vp = {s = glue (linVP (VStem pol.p) vp) "기" ++ "위해"} ;
    CompVP ant pol pron vp = emptyComp ** {
      s = \\vf => linVP (VAttr pol.p) vp ++ "것" ++ copula.s ! vf
      } ;
    UttVP ant pol pron vp = {s = pron.s ! Subject ++ linVP (VF Formal pol.p) vp} ;

    ReflA2 a rnp = a ** {compar = rnp.s ++ a.p2.s ! Vowel} ;
    ReflVPSlash slash rnp = insertComp slash (stringNP rnp.s) ;
    RecipVPSlash slash = insertComp slash (stringNP "서로") ;
    RecipVPSlashCN slash cn = insertComp slash
      (stringNP ("서로의" ++ cn.s ! Bare)) ;

    FocusComp comp np = predVP np (G.UseComp comp) ;
    whatSgFem_IP = stringNP "무엇" ;
    whatSgNeut_IP = stringNP "무엇" ;

lin that_RP = G.IdRP ;

}
