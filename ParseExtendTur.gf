concrete ParseExtendTur of ParseExtend =
  ExtendTur - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralTur - [num], PunctuationX ** 
  open Prelude, ResTur, HarmonyTur, GrammarTur in {

lincat CNN = {
  s1,s2 : Case => Str;
  n : Number
} ;

lin
    gen_Quant = {s = []; useGen = NoGen} ;

    UttAP p ap = {
      s = (CompAP ap).s ! Perf ! VFin Pres Simul Pos p.a
    } ;
    UttVPS p vps = {s = p.s ! Nom ++ vps.s ! p.a} ;
    UttVP ant pol p vp = {
      s = vp.compl ++ vp.s ! Perf ! VInf pol.p
    } ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin TimeNP np = {s = np.s ! Nom} ;

lin BaseCNN n1 cn1 n2 cn2 = {
      s1 = \\c => n1.s ! n1.n ! Nom ++ cn1.s ! n1.n ! c;
      s2 = \\c => n2.s ! n2.n ! Nom ++ cn2.s ! n2.n ! c;
      n = Pl
    } ;

lin DetCNN quant conj cnn = {
      s = \\c => quant.s ++ cnn.s1 ! c ++ conj.s ++ cnn.s2 ! c;
      h = {vow=I_Har; con=SCon Soft};
      a = agrP3 cnn.n
    } ;

lin ReflPossCNN conj cnn = {s = \\_,c => cnn.s1 ! c ++ conj.s ++ cnn.s2 ! c} ;

lin PossCNN_RNP quant conj cnn rnp = {
      s = \\a,c => quant.s ++ cnn.s1 ! c ++ conj.s ++ cnn.s2 ! c ++
                    conj.s ++ rnp.s ! a ! c
    } ;

lin ReflA2 ap rnp = {
      s = \\n,c => rnp.s ! (agrP3 n) ! ap.c.c ++ ap.c.s ++ ap.s ! n ! c;
      h = ap.h
    } ;

lin ReflVPSlash vp rnp = {
      s = mkVerbForms vp;
      compl = vp.compl ++ rnp.s ! (agrP3 Sg) ! vp.c.c ++ vp.c.s
    } ;

lin NumLess n = n ** {s = \\numb,c => n.s ! numb ! c ++ "eksik"} ;
lin NumMore n = n ** {s = \\numb,c => n.s ! numb ! c ++ "fazla"} ;

lin UseACard card = {s = \\_,_ => card.s} ;
lin UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s} ;

lin RelNP np rs = {
      s = \\c => rs.s ! np.a ++ np.s ! c;
      h = np.h;
      a = np.a
    } ;

lin ExtRelNP np rs = {
      s = \\c => rs.s ! np.a ++ "," ++ np.s ! c;
      h = np.h;
      a = np.a
    } ;

lin ExtAdvAP ap adv = {
      s = \\n,c => ap.s ! n ! c ++ "," ++ adv.s;
      h = ap.h
    } ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ comp.compl ++
          comp.s ! Perf ! VFin Pres Simul Pos (agrP3 Sg) ++
          cadv.p ++ adv.s ++ cadv.s
    } ;

lin CAdvAP pol cadv ap comp = {
      s = \\n,c => pol.s ++ comp.compl ++
          comp.s ! Perf ! VFin Pres Simul Pos (agrP3 Sg) ++
          cadv.p ++ cadv.s ++ ap.s ! n ! c;
      h = ap.h
    } ;

lin AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.p; c = cadv.c} ;

lin EnoughAP ap ant pol vp = {
      s = \\n,c => vp.compl ++ vp.s ! Perf ! VInf pol.p ++ "için" ++
                  "yeterince" ++ ap.s ! n ! c;
      h = ap.h
    } ;

lin EnoughAdv adv = {s = "yeterince" ++ adv.s} ;
lin AdvAdv adv mod = {s = mod.s ++ adv.s} ;

lin whatSgFem_IP = {s = "ne"} ;
lin whatSgNeut_IP = {s = "ne"} ;

lin EmbedVP ant pol p vp = {
      s = vp.compl ++ vp.s ! Perf ! VInf pol.p
    } ;

lin ComplVV vv ant pol vp = {
      s = mkVerbForms vv;
      compl = vp.compl ++ vp.s ! Perf ! VInf pol.p
    } ;

lin SlashVV vv ant pol vp = vv ** {
      compl = vp.compl ++ mkVerbForms vp ! Perf ! VInf pol.p;
      c = vp.c
    } ;

lin SlashV2V vv ant pol vp = vv ** {
      compl = vp.compl ++ vp.s ! Perf ! VInf pol.p;
      c = vv.c
    } ;

lin SlashV2VNP vv np ant pol vp = vv ** {
      compl = np.s ! vv.c.c ++ vv.c.s ++ vp.compl ++
              mkVerbForms vp ! Perf ! VInf pol.p;
      c = vp.c
    } ;

lin InOrderToVP ant pol p vp = {
      s = vp.compl ++ vp.s ! Perf ! VInf pol.p ++ "için"
    } ;

lin CompVP ant pol p vp = {
      s = \\_,_ => vp.compl ++ vp.s ! Perf ! VInf pol.p;
      compl = []
    } ;

lin RecipVPSlash vp = {
      s = mkVerbForms vp;
      compl = vp.compl ++ "birbirini" ++ vp.c.s
    } ;

lin RecipVPSlashCN vp cn = {
      s = mkVerbForms vp;
      compl = vp.compl ++ "birbirinin" ++ cn.s ! Pl ! vp.c.c ++ vp.c.s
    } ;

lin FocusComp comp np = {
      s = \\t,a,p => comp.compl ++ comp.s ! Perf ! VFin t a p np.a ++ np.s ! Nom
    } ;

lin num x = x ;

lin BareN2 n = n ;

lin that_RP = GrammarTur.IdRP ;

}
