concrete ParseExtendUrd of ParseExtend =
  ExtendUrd - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralUrd - [num], PunctuationX **
  open Prelude, CommonHindustani, ResUrd, (G=GrammarUrd), (E=ExtendUrd) in {

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin that_RP = G.IdRP ;

lin gen_Quant = {s = \\_,_,_ => [] ; a = defaultAgr} ;

    UttAP pron ap = {s = ap.s ! giveNumber pron.a ! giveGender pron.a ! Dir ! Posit} ;
    UttVPS pron vps = {s = np2pronCase pron.s vps.c pron.a ++ vps.s ! pron.a} ;

lincat CNN = {s1,s2 : Case => Str ; n1,n : Number ; g1 : Gender} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\c => num1.s ++ cn1.s ! num1.n ! c ;
      s2 = \\c => num2.s ++ cn2.s ! num2.n ! c ;
      n1 = num1.n ; n = conjNumber num1.n num2.n ; g1 = cn1.g
      } ;
    DetCNN quant conj cnn = {
      s = \\c => quant.s ! cnn.n1 ! cnn.g1 ! caseOfNP c ++ cnn.s1 ! caseOfNP c ++
                  conj.s2 ++ cnn.s2 ! caseOfNP c ;
      a = agrP3 cnn.g1 cnn.n
      } ;
    ReflPossCNN conj cnn = {
      s = \\c => "اپنے" ++ cnn.s1 ! caseOfNP c ++ conj.s2 ++ cnn.s2 ! caseOfNP c ;
      a = agrP3 cnn.g1 cnn.n ; lock_NP = <>
      } ;
    PossCNN_RNP quant conj cnn rnp = {
      s = \\c => rnp.s ! NPC Obl ++ "کے" ++ quant.s ! cnn.n1 ! cnn.g1 ! caseOfNP c ++
                  cnn.s1 ! caseOfNP c ++ conj.s2 ++ cnn.s2 ! caseOfNP c ;
      a = agrP3 cnn.g1 cnn.n ; lock_NP = <>
      } ;

lin NumLess num = {s = num.s ++ "کم" ; n = num.n} ;
    NumMore num = {s = num.s ++ "زیادہ" ; n = num.n} ;
    UseACard acard = {s = acard.s ; n = Pl} ;
    UseAdAACard ada acard = {s = ada.s ++ acard.s ; n = Pl} ;

lin RelNP np rs = {s = \\c => np.s ! c ++ rs.s ! np.a ; a = np.a} ;
    ExtRelNP np rs = {s = \\c => np.s ! c ++ comma ++ rs.s ! np.a ; a = np.a} ;
    ExtAdvAP ap adv = {s = \\n,g,c,d => ap.s ! n ! g ! c ! d ++ comma ++ adv.s ! g} ;
    BareN2 n2 = {s = n2.s ; g = n2.g} ;

lin ComparAdv pol cadv adv comp = {
      s = \\g => neg pol ++ comp.s ! defaultAgr ++ cadv.p ++ cadv.s ++ adv.s ! g
      } ;
    CAdvAP pol cadv ap comp = {
      s = \\n,g,c,d => neg pol ++ comp.s ! agrP3 g n ++ cadv.p ++ cadv.s ++ ap.s ! n ! g ! c ! d
      } ;
    AdnCAdv pol cadv = {s = neg pol ++ cadv.s ++ cadv.p ; p = False} ;
    EnoughAP ap ant pol vp = {
      s = \\n,g,c,d => ap.s ! n ! g ! c ! d ++ "اتنا" ++ infClause ant pol (agrP3 g n) vp
      } ;
    EnoughAdv adv = {s = \\g => adv.s ! g ++ "کافی"} ;

lin TimeNP np = {s = \\_ => np.s ! NPC Dir} ;
    AdvAdv first second = {s = \\g => first.s ! g ++ second.s ! g} ;
    whatSgFem_IP = G.whatSg_IP ** {g = Fem} ;
    whatSgNeut_IP = G.whatSg_IP ** {g = Masc} ;

lin EmbedVP ant pol pron vp = {s = ant.s ++ neg pol ++ infOblique vp pron.a ; fromVP = True} ;
    ComplVV vv ant pol vp =
      insertTrans (insertVV (infComplement vv.isAux ant pol vp) (predV vv) vp.embComp) VTrans ;
    SlashVV vv ant pol vp =
      insertEmbCompl (insertObj (\\a => infComplement vv.isAux ant pol vp) (predV vv)) vp.embComp **
        {c2 = vp.c2} ;
    SlashV2V v ant pol vp =
      insertVV (infComplement v.isAux ant pol vp) (predV v) vp.embComp **
        {c2 = {s = v.c2 ; c = VTrans}} ;
    SlashV2VNP vv np ant pol vp =
      insertObjPre (\\_ => np.s ! NPObj)
        (insertObjc (\\a => infComplement vv.isAux ant pol vp) (predVc vv)) ** {c2 = vp.c2} ;
    InOrderToVP ant pol pron vp = {s = \\_ => infClause ant pol pron.a vp ++ "کے لیے"} ;
    CompVP ant pol pron vp = {s = \\_ => infClause ant pol pron.a vp} ;
    UttVP ant pol pron vp = {s = infClause ant pol pron.a vp} ;
    ReflA2 = E.ReflA2RNP ;
    ReflVPSlash = E.ReflRNP ;

lin RecipVPSlash slash = G.ComplSlash slash reciprocalNP ;
    RecipVPSlashCN slash cn =
      G.ComplSlash slash (G.DetCN (G.DetQuant gen_Quant G.NumSg) (G.PossNP cn reciprocalNP)) ;
    FocusComp comp np = G.PredVP np (G.UseComp comp) ;
    num x = {s = table {NCard => x.s ; NOrd => x.s ++ "واں"} ; n = x.n} ;

oper
  caseOfNP : NPCase -> Case = \c -> case c of {NPC x => x ; NPObj => Obl ; NPErg => Obl} ;
  neg : Pol -> Str = \pol -> case pol.p of {Pos => [] ; Neg => "نہ"} ;
  infClause : Ant -> Pol -> Agr -> VPH -> Str = \ant,pol,agr,vp ->
    ant.s ++ neg pol ++ infVP False vp agr ;
  infComplement : Bool -> Ant -> Pol -> VPH -> Str = \isAux,ant,pol,vp ->
    ant.s ++ neg pol ++ infVV isAux vp ;
  infOblique : VPH -> Agr -> Str = \vp,agr ->
    vp.obj.s ++ vp.ad ++ vp.comp ! agr ++ vp.cvp ++ (vp.s ! VPInf).fin ;
  reciprocalNP : NP = {s = \\_ => "ایک دوسرے" ; a = agrP3 Masc Pl ; lock_NP = <>} ;

}
