concrete ParseExtendGer of ParseExtend =
  ExtendGer - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash, ReflA2RNP], NumeralGer - [num], PunctuationX **
 open Prelude, ResGer, (G=GrammarGer), (E=ExtendGer) in {

oper
    useInfVPPol : Bool -> Ant -> Pol -> Agr -> ResGer.VP -> Str =
      \isAux,ant,pol,agr,vp ->
        let
          vpi = infVP isAux ant.a pol.p vp ;
          glue : (Agr => Str) * Str -> Str = \i -> i.p1 ! agr ++ i.p2
        in
          glue (embedInf vpi.inpl <vpi.objs, vpi.pred>) ++ vpi.extr ! agr ++ vp.ext ;

    recipNP : ResGer.NP = {
      s = \\_,_ => "einander" ;
      a = AgPl P3 ;
      w = WPron ;
      rc, ext = []
      } ;

    recipPossNP : CN -> ResGer.NP = \cn -> {
      s = \\_,c => artDef ! (gennum cn.g Sg) ! c ++ cn.s ! Weak ! Sg ! c
                  ++ cn.adv ++ "voneinander" ;
      a = agrgP3 cn.g Sg ;
      w = WHeavy ;
      rc = cn.rc ! Sg ;
      ext = cn.ext
      } ;

lincat CNN = {
    s1, s2 : Adjf => Case => Str ;
    n1, n2, n : Number ;
    g1, g2 : Gender
    } ;

lin
    gen_Quant = G.DefArt ;

    UttAP  p ap = {s = ap.c.p1 ++ ap.s ! APred ++ ap.c.p2 ++ ap.ext} ;
    UttVPS p vps= let vpss = vps.s ! Main ! p.a in {s = vpss.verb ++ vpss.compl} ; --- is the order as intended?

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    FocusComp comp np =
      let
        vp = insertExtrapos (comp.ext ! numberAgr np.a) (insertObj comp.s (predV sein_V)) ;
        subj = mkSubject np vp.c1
      in
        mkClause subj.s subj.a vp ;

    BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\a,c => num1.s ! agrAdj a (gennum cn1.g num1.n) c
                    ++ cn1.s ! a ! num1.n ! c ++ cn1.adv ++ cn1.ext ++ cn1.rc ! num1.n ;
      s2 = \\a,c => num2.s ! agrAdj a (gennum cn2.g num2.n) c
                    ++ cn2.s ! a ! num2.n ! c ++ cn2.adv ++ cn2.ext ++ cn2.rc ! num2.n ;
      n1 = num1.n ;
      n2 = num2.n ;
      n = conjNumber num1.n num2.n ;
      g1 = cn1.g ;
      g2 = cn2.g
      } ;

    DetCNN quant conj cnn = {
      s = \\b,c => quant.s ! b ! (gennum cnn.g1 cnn.n1) ! c
                  ++ cnn.s1 ! quant.a ! c
                  ++ conj.s1
                  ++ conj.s2
                  ++ quant.s ! False ! (gennum cnn.g2 cnn.n2) ! c
                  ++ cnn.s2 ! quant.a ! c ;
      a = agrgP3 cnn.g1 (conjNumber conj.n cnn.n) ;
      w = case quant.isDefArt of {True => WDefArt ; False => WHeavy} ;
      rc, ext = []
      } ;

    ReflPossCNN conj cnn = {
      s = \\a,c => possPron a cnn.n1 cnn.g1 c
                  ++ cnn.s1 ! (case cnn.n1 of {Sg => Strong ; Pl => Weak}) ! c
                  ++ conj.s1
                  ++ conj.s2
                  ++ cnn.s2 ! (case cnn.n2 of {Sg => Strong ; Pl => Weak}) ! c ;
      rc, ext = [] ;
      isPron = False
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\a,c => quant.s ! False ! (gennum cnn.g1 cnn.n1) ! c
                  ++ cnn.s1 ! quant.a ! c
                  ++ conj.s1
                  ++ conj.s2
                  ++ quant.s ! False ! (gennum cnn.g2 cnn.n2) ! c
                  ++ cnn.s2 ! quant.a ! c
                  ++ appPrep vonDat (rnp.s ! a) ++ rnp.ext ++ rnp.rc ;
      rc, ext = [] ;
      isPron = False
      } ;

    NumLess num = num ** {
      s, sp = \\_ => num.s ! APred ++ "weniger" ;
      n = Pl ;
      isNum = False
      } ;

    NumMore num = num ** {
      s, sp = \\af => num.s ! APred ++ (regA "weiter").s ! Posit ! af ;
      n = Pl ;
      isNum = False
      } ;

    UseACard card = {s = \\_ => card.s ; n = card.n} ;
    UseAdAACard ada card = {s = \\_ => ada.s ++ card.s ; n = card.n} ;

    ComparAdv pol cadv adv comp = {
      s = negation ! pol.p ++ cadv.s ++ adv.s ++ cadv.p
          ++ comp.s ! agrP3 Sg ++ comp.ext ! Sg
      } ;

    CAdvAP pol cadv ap comp = ap ** {
      s = \\af => negation ! pol.p ++ cadv.s ++ ap.s ! af ;
      s2 = \\_ => cadv.p ++ comp.s ! agrP3 Sg ++ comp.ext ! Sg ;
      isPre = True
      } ;

    AdnCAdv pol cadv = {s = negation ! pol.p ++ cadv.s ++ cadv.p} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\af => ap.s ! af ++ "genug" ;
      isPre = False ;
      ext = ap.ext ++ "um" ++ useInfVPPol False ant pol (agrP3 Sg) vp
      } ;

    EnoughAdv adv = {s = adv.s ++ "genug"} ;

    ExtAdvAP ap adv = ap ** {
      s = \\af => ap.s ! af ++ embedInCommas adv.s ;
      isPre = False
      } ;

    TimeNP np = {s = np.s ! False ! Acc ++ bigNP np} ;

    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

    whatSgFem_IP = {
      s = \\c => "welch" + detEnding ! (GSg Fem) ! c ;
      a = GSg Fem ;
      isPron = True
      } ;
    whatSgNeut_IP = G.whatSg_IP ;

lin SlashV2VNP v np ant pol vp =   -- bitte ihn, zu kaufen | lasse ihn kaufen   HL 3/22
      let prep = v.c2 ;
          obj = appPrep prep (np.s!False) ; -- simplify: no glueing of prep+DefArt, HL 8/22
          b : Bool = case prep.t of {isPrep | isPrepDefArt => True ; _ => False} ;
          c = prep.c ;
          w = np.w ;
          vps = (ComplVV v ant pol vp ** {c2 = vp.c2 ; objCtrl = vp.objCtrl})
      in
      insertObj' obj b w c vps ;

    ComplVV v ant pol vp = -- HL 3/22: leave inf-complement in-place, extract infzu-complement
      let
        vps = predVGen v.isAux v ; -- e.g. will.isAux=True | wagt.isAux=False
        inf = mkInf v.isAux ant.a pol.p vp
      in
      insertExtrapos vp.ext (insertInf inf vps) ;

    SlashVV v ant pol vp =
      let
        vps = predVGen v.isAux v ;
        vpi = infVPSlash v.isAux ant.a pol.p vp ;
        topInpl = <vpi.objs, vpi.pred> ;
        emptyInpl : (Agr => Str) * Str = <\\_ => [], []> ;
        inf : {inpl : (Agr => Str) * Str ; extr : Agr => Str} =
          case <v.isAux,vp.isAux> of {
            <False,True> =>
              {inpl = emptyInpl ;
               extr = let moved = embedInf vpi.inpl topInpl
                      in \\agr => (glueInpl moved) ! agr ++ vpi.extr ! agr} ;
            _ =>
              {inpl = embedInf vpi.inpl topInpl ;
               extr = vpi.extr}
            }
      in
        insertExtrapos vp.ext (insertInf inf vps) ** {c2 = vp.c2 ; objCtrl = vp.objCtrl} ;

    SlashV2V v ant pol vp =
      let
        vps = predVGen v.isAux v ;
        inf = mkInf v.isAux ant.a pol.p vp
      in
        insertExtrapos vp.ext (insertInf inf vps) ** {c2 = v.c2 ; objCtrl = v.objCtrl} ;

    InOrderToVP ant pol p vp = {s = "um" ++ useInfVPPol False ant pol p.a vp} ;

    CompVP ant pol p vp = {
      s = \\_ => useInfVPPol False ant pol p.a vp ;
      ext = \\_ => []
      } ;

    EmbedVP ant pol p vp = {s = useInfVPPol False ant pol p.a vp} ;

    UttVP ant pol p vp = {s = useInfVPPol False ant pol p.a vp} ;

    ReflA2 = E.ReflA2RNP ;
    ReflVPSlash = E.ReflRNP ;

    RecipVPSlash slash = G.ComplSlash slash recipNP ;
    RecipVPSlashCN slash cn = G.ComplSlash slash (recipPossNP cn) ;

lin num x = x ;

lin RelNP = G.RelNP ;
    ExtRelNP = G.RelNP ;

lin BareN2 n = n ;

lin that_RP = G.IdRP ;

}
