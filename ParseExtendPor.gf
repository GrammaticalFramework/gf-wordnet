concrete ParseExtendPor of ParseExtend =
  ExtendPor - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralPor - [num], PunctuationX **
  open Prelude, ResPor, MorphoPor, GrammarPor, (E = ExtraPor), Coordination in {

  lin
    --^
    gen_Quant = {
      s = \\b,n,g,c => "" ;
      s2 = "" ;
      sp = \\n,g,c => artDef False g n c ;
      isNeg = False
      } ;

    UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

  lin
    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

  --^ RNP is not implemented yet in ExtendPor
  --lin AdvRNP np prep rnp = rnp ;

  lincat
    -- True if digit
    CNN = {s1 : Bool => Str ; s2 : Str; n1,n : Number; g1 : Gender} ;
  lin
    BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\d => num1.s ! cn1.g ++ cn1.s ! num1.n ;
      s2 = num2.s ! cn2.g ++ cn2.s ! num2.n ;
      n1 = num1.n ;
      g1 = cn1.g ;
      n = conjNumber num1.n num2.n ;
      } ;

  lin
    NumLess n = {s = \\g => n.s ! g ++ "menos" ;
                 n = n.n ; -- shouldn't it be 2 - ?
                 isNum = n.isNum} ;

    NumMore n = {s = \\g => n.s ! g ++ "mais" ;
                 n = n.n ; isNum = n.isNum } ;

  lincat Sub1000000000 = {s : CardOrd => Str ; n : Number} ;

  lin
    pot3as4 n = n ;

    pot4 n = {s = table CardOrd {co => n.s ! NCard Masc ++ milhao ! co } ; n = Pl} ;

    pot4plus n m = {s = \\co => n.s ! NCard Masc
                      ++ milhao ! co
                      ++ e_CardOrd co ++ m.s ! co ;
                    n = Pl
      } ;

    pot21 = mkNum "cem" "centésimo" ;

    pot31 = mkNum "mil" "milésimo" ;

    -- cem, mil, but um milhão, um bilhão
    pot41 = mkNum "um milhão" "milhonésimo" ;

  oper
    milhao : CardOrd => Str ;
    milhao = mkNumStr "milhão" "milhonésimo" ;

    mkNum : Str -> Str -> {s : CardOrd => Str ; n : Number} ;
    mkNum cem centesimo = spl (mkNumStr cem centesimo) ;

  lin
    num x = x ;

  lin
    UseACard ac = {
      s = \\_g => ac.s ;
      n = Pl
      } ;

    UseAdAACard ada ac = {
      s = \\_g => ada.s ;
      n = Sg
      } ;

  lin
    RelNP = GrammarPor.RelNP ;

    ExtRelNP np rs = heavyNPpol np.isNeg {
      s = \\c => (np.s ! c).ton ++ bindComma ++ rs.s ! Indic ! np.a ;
      a = np.a ;
      hasClit = False
      } ;

  lin ExtAdvAP ap adv = ap ** {
        s = \\a => ap.s ! a ++ bindComma ++ adv.s
        } ;

  lin BareN2 n2 = n2 ;

  lin
    --TODO: test and probably correct use of comp in the following
    --TODO: create oper for neg pattern
    ComparAdv pol cadv adv comp = let
      neg = (negation ! pol.p).p1
      in {
        s = pol.s ++ neg ++ cadv.s ++ adv.s ++ comp.s ! Ag Masc Sg P3
      } ;

    CAdvAP pol cadv ap comp = let
      neg = (negation ! pol.p).p1
      in ap ** {
        s = \\af => pol.s ++ neg ++ cadv.s ++ ap.s ! af ++ comp.s ! Ag Masc Sg P3
      } ;

    AdnCAdv pol cadv = let
      neg = (negation ! pol.p).p1
      in {
        s = pol.s ++ neg ++ cadv.s ++ "que"
      } ;

  lin
    EnoughAP ap ant pol vp = {
      s = \\af => let g : Gender = aform2gender af ;
                      n : Number = aform2number af
        in ap.s ! af ++ "o suficiente" ++ ant.s ++ pol.s ++ infVP (Ag g n P3)
      } ** ap ;

    -- not sure about this, but in the one example seems to work
    EnoughAdv adv = adv ;

  lin
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

  lin
    whatSgFem_IP = whatSg_IP ** {a = aagr Fem Sg} ;
    whatSgNeut_IP = whatSg_IP ;

  --TODO: check
  lin that_RP = {
        s = relPron ;
        a = aagr Masc Sg ;
        hasAgr = False
        } ;

  lin
    ComplVV vv ant pol vp = let
      neg = (negation ! pol.p).p1 ;
      vf : Agr -> Str = \agr -> case ant.a of {
        Simul => infVP vp agr ;
        Anter => nominalVP (\_ -> VFin (VPres Indic) agr.n agr.p) vp agr
        } ;
      in
      insertComplement (\\a => ant.s ++ pol.s ++ neg ++ prepCase vv.c2.c ++ vf a) (predV vv) ;
    UttVP ant pol p vp = let
      neg = (negation ! pol.p).p1
      in {
        s = ant.s ++ pol.s ++ neg ++ infVP vp p.a
      } ;

  lin FocusComp comp np = mkClause (comp.s ! np.a) np.hasClit np.isPol np.a (insertComplement (\\_ => (np.s ! Nom).ton) (predV (selectCopula comp.cop))) ;

} ;
