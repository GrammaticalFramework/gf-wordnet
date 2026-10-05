concrete ParseExtendLav of ParseExtend =
  ExtendLav - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralLav - [num], PunctuationX **
  open Prelude, ResLav, ParadigmsLav, (G=GrammarLav) in {

lincat CNN = {
  s1, s2 : Case => Str ;
  agr1, agr2 : Agreement
} ;

lin gen_Quant = {s = \\_,_,_ => [] ; defin = Indef ; pol = Pos} ;

lin UttVPS p vps = {s = vps.s ! p.agr} ;

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin UttAP p ap = { s = let a = fromAgr p.agr in ap.s ! Indef ! a.gend ! a.num ! Nom } ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\c => num1.s ! cn1.gend ! c ++ cn1.s ! Indef ! num1.num ! c ;
      s2 = \\c => num2.s ! cn2.gend ! c ++ cn2.s ! Indef ! num2.num ! c ;
      agr1 = AgrP3 num1.num cn1.gend ;
      agr2 = AgrP3 num2.num cn2.gend
    } ;

    DetCNN quant conj cnn = {
      s = \\c => let a = fromAgr cnn.agr1 in
        quant.s ! a.gend ! a.num ! c ++ conj.s1 ++ cnn.s1 ! c ++
        conj.s2 ++ cnn.s2 ! c ;
      agr = toAgr (fromAgr (conjAgr cnn.agr1 cnn.agr2)).pers
                  (conjNumber (fromAgr cnn.agr1).num conj.num)
                  (fromAgr (conjAgr cnn.agr1 cnn.agr2)).gend ;
      pol = quant.pol ; isRel = False ; isPron = False
    } ;

    ReflPossCNN conj cnn = {
      s = \\agr,c => "savs" ++ conj.s1 ++ cnn.s1 ! c ++ conj.s2 ++ cnn.s2 ! c ;
      isPron = False
    } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,c => let a = fromAgr cnn.agr1 in
        quant.s ! a.gend ! a.num ! c ++ conj.s1 ++ cnn.s1 ! c ++
        conj.s2 ++ cnn.s2 ! c ++ rnp.s ! agr ! Gen ;
      isPron = False
    } ;

lin NumMore num = {s = \\g,c => "vēl" ++ num.s ! g ! c ; num = Pl ; hasCard = True} ;
    NumLess num = {s = \\g,c => "mazāk" ++ num.s ! g ! c ; num = Pl ; hasCard = True} ;

    UseACard card = {s = \\_,_ => card.s ; num = Pl} ;
    UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s ; num = Pl} ;

lin RelNP np rs = {
      s = \\c => np.s ! c ++ rs.s ! np.agr ; agr = np.agr ; pol = np.pol ;
      isRel = True ; isPron = False
    } ;
    ExtRelNP np rs = {
      s = \\c => np.s ! c ++ "," ++ rs.s ! np.agr ; agr = np.agr ; pol = np.pol ;
      isRel = True ; isPron = False
    } ;

lin BareN2 n2 = {s = n2.s ; gend = n2.gend} ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.prep ++ comp.s ! AgrP3 Sg Masc ;
      isPron = False
    } ;
    CAdvAP pol cadv ap comp = {
      s = \\d,g,n,c => pol.s ++ cadv.s ++ ap.s ! d ! g ! n ! c ++
                        cadv.prep ++ comp.s ! AgrP3 n g
    } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.prep} ;

    EnoughAP ap ant pol vp = {
      s = \\d,g,n,c => "pietiekami" ++ ap.s ! d ! g ! n ! c ++ "lai" ++
                        ant.s ++ pol.s ++ buildVP vp pol.p VInf (AgrP3 n g)
    } ;
    EnoughAdv adv = {s = "pietiekami" ++ adv.s ; isPron = False} ;
    ExtAdvAP ap adv = {
      s = \\d,g,n,c => ap.s ! d ! g ! n ! c ++ "," ++ adv.s
    } ;

lin TimeNP np = {s = np.s ! Acc ; isPron = False} ;
    AdvAdv x y = {s = x.s ++ y.s ; isPron = False} ;

lin whatSgFem_IP = G.whatSg_IP ;
    whatSgNeut_IP = G.whatSg_IP ;

lin ComplVV vv ant pol vp = {
      v        = vv ;
      compl    = \\agr => ant.s ++ pol.s ++ buildVP vp pol.p VInf agr ;
      voice    = Act ;
      leftVal  = vv.leftVal ;
      rightAgr = AgrP3 Sg Masc ;
      rightPol = Pos ;
      objPron  = False
    } ;

    SlashVV vv ant pol vp = {
      v = vv ;
      compl = \\agr => ant.s ++ pol.s ++ buildVP vp pol.p VInf agr ;
      voice = Act ; leftVal = vv.leftVal ; rightAgr = vp.rightAgr ;
      rightPol = vp.rightPol ; objPron = False ; rightVal = vp.rightVal
    } ;

    SlashV2V v2v ant pol vp = {
      v        = v2v ;
      compl    = \\agr => ant.s ++ pol.s ++ buildVP vp pol.p VInf agr ;
      voice    = Act ;
      leftVal  = v2v.leftVal ;
      rightAgr = AgrP3 Sg Masc ;
      rightPol = Pos ;
      objPron  = False ;  -- will be overriden
      rightVal = v2v.rightVal
    } ;

    SlashV2VNP v2v np ant pol vp = {
      v = v2v ;
      compl = \\agr => v2v.rightVal.s ++
        np.s ! (v2v.rightVal.c ! (fromAgr np.agr).num) ++
        ant.s ++ pol.s ++ buildVP vp pol.p VInf agr ;
      voice = Act ; leftVal = v2v.leftVal ; rightAgr = np.agr ;
      rightPol = np.pol ; objPron = np.isPron ; rightVal = vp.rightVal
    } ;

lin InOrderToVP ant pol p vp = {
      s = "lai" ++ ant.s ++ pol.s ++ buildVP vp pol.p VInf p.agr ;
      isPron = False
    } ;

    CompVP ant pol p vp = {
      s = \\_ => ant.s ++ pol.s ++ buildVP vp pol.p VInf p.agr
    } ;

    UttVP ant pol p vp = {s = ant.s ++ pol.s ++ buildVP vp pol.p VInf p.agr} ;

lin ReflA2 a rnp = {
      s = \\d,g,n,c => a.s ! (AAdj Posit d g n c) ++ a.prep.s ++
                        rnp.s ! AgrP3 n g ! (a.prep.c ! n)
    } ;
    ReflVPSlash vp rnp = insertObjPre
      (\\agr => vp.rightVal.s ++
                rnp.s ! agr ! (vp.rightVal.c ! (fromAgr agr).num)) vp ;

lin RecipVPSlash vp = insertObjPre
      (\\agr => vp.rightVal.s ++ "viens otru") vp ;
    RecipVPSlashCN vp cn = insertObjPre
      (\\agr => vp.rightVal.s ++ "viens otra" ++
                cn.s ! Def ! (fromAgr agr).num ! Acc) vp ;

lin FocusComp comp np = {
      s = \\mood,pol => comp.s ! np.agr ++ np.s ! Nom ++
                         (mkV "būt").s ! pol !
                           (VInd (fromAgr np.agr).pers (fromAgr np.agr).num
                             (case mood of {Ind _ t => t ; _ => Pres}))
    } ;

lin EmbedVP ant pol p vp = { s = ant.s ++ pol.s ++ buildVP vp pol.p VInf p.agr } ;

lin num x = x ;

lin that_RP = G.IdRP ;

}
