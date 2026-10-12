concrete ParseExtendPor of ParseExtend =
  ExtendPor - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralPor - [num], PunctuationX **
  open Prelude, ResPor, MorphoPor, GrammarPor, (E = ExtraPor), Coordination, ExtendPor in {

  lin
    --^
    gen_Quant = {
      s = \\b,n,g,c => "" ;
      s2 = "" ;
      sp = \\n,g,c => artDef False g n c ;
      spn = \\c => artDef False Masc Sg c ;
      isNeg = False
      } ;

    UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

  lin
    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

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

    DetCNN quant conj cnn = heavyNPpol quant.isNeg {
      s = \\c => quant.s ! False ! cnn.n1 ! cnn.g1 ! c ++
                  conj.s1 ++ cnn.s1 ! False ++ conj.s2 ++ cnn.s2 ;
      a = agrP3 cnn.g1 cnn.n ;
      hasClit = False
      } ;

    ReflPossCNN conj cnn = {
      s = \\agr,c => possCase cnn.g1 cnn.n1 c ++
                      reflPossDet agr cnn.n1 cnn.g1 ++
                      conj.s1 ++ cnn.s1 ! False ++ conj.s2 ++ cnn.s2 ;
      isPron = False
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,c => quant.s ! False ! cnn.n1 ! cnn.g1 ! c ++
                      conj.s1 ++ cnn.s1 ! False ++ rnp.s ! agr ! genitive ++
                      conj.s2 ++ cnn.s2 ++ rnp.s ! agr ! genitive ;
      isPron = False
      } ;

  lin
    NumLess n = {s = \\g => n.s ! g ++ "menos" ;
                 n = n.n ; -- shouldn't it be 2 - ?
                 isNum = n.isNum} ;

    NumMore n = {s = \\g => n.s ! g ++ "mais" ;
                 n = n.n ; isNum = n.isNum } ;

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
    EnoughAP ap ant pol vp = ap ** {
      s = \\af => let g : Gender = aform2gender af ;
                      n : Number = aform2number af
        in ap.s ! af ++ "o suficiente" ++ ant.s ++ pol.s ++ infVP vp pol.p (Ag g n P3)
      } ;

    -- not sure about this, but in the one example seems to work
    EnoughAdv adv = adv ;

    TimeNP np = {s = (np.s ! Nom).ton} ;

  lin
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

  lin
    whatSgFem_IP = whatSg_IP ** {a = aagr Fem Sg} ;
    whatSgNeut_IP = whatSg_IP ;

    InOrderToVP ant pol p vp = {
      s = "para" ++ ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;

  --TODO: check
  lin that_RP = {
        s = relPron ;
        a = aagr Masc Sg ;
        hasAgr = False
        } ;

  lin
    EmbedVP ant pol p vp = {
        s = \\c => prepCase c ++ ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;
    ComplVV vv ant pol vp = let
      vf : Agr -> Str = \agr -> case ant.a of {
        Simul => infVP vp pol.p agr ;
        Anter => nominalVP (\_ -> VFin (VPres Indic) agr.n agr.p) vp pol.p agr
        }
      in
      insertComplement (\\a => ant.s ++ pol.s ++ prepCase vv.c2.c ++ vf a) (predV vv) ;
    SlashVV vv ant pol slash = let
      vf : Agr -> Str = \agr -> case ant.a of {
        Simul => infVP slash pol.p agr ;
        Anter => nominalVP (\_ -> VFin (VPres Indic) agr.n agr.p) slash pol.p agr
        }
      in
      insertComplement (\\a => prepCase vv.c2.c ++ ant.s ++ pol.s ++ vf a) (predV vv) **
        {c2 = slash.c2} ;
    SlashV2V v ant pol vp = let
      vf : Agr -> Str = \agr -> case ant.a of {
        Simul => infVP vp pol.p agr ;
        Anter => nominalVP (\_ -> VFin (VPres Indic) agr.n agr.p) vp pol.p agr
        }
      in
      mkVPSlash v.c2
        (insertComplement (\\a => v.c3.s ++ prepCase v.c3.c ++ ant.s ++ pol.s ++ vf a)
                          (predV v)) ;
    SlashV2VNP v np ant pol slash = let
      vf : Agr -> Str = \agr -> case ant.a of {
        Simul => infVP slash pol.p agr ;
        Anter => nominalVP (\_ -> VFin (VPres Indic) agr.n agr.p) slash pol.p agr
        }
      in
      mkVPSlash slash.c2
        (insertComplement (\\a => v.c3.s ++ prepCase v.c3.c ++ ant.s ++ pol.s ++ vf a)
                          (insertObject v.c2 np (predV v))) ;
    CompVP ant pol p vp = {
        s = \\agr => ant.s ++ pol.s ++ "de" ++ infVP vp pol.p p.a ;
        cop = serCopula
      } ;
    UttVP ant pol p vp = {
        s = ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;

    ReflVPSlash = ExtendPor.ReflRNP ;
    ReflA2 a rnp = a ** {
      s = \\af => a.s ! af ++ a.c2.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! a.c2.c ;
      isPre = False
      } ;
    RecipVPSlash slash = ExtendPor.ReflRNP slash ExtendPor.ReflPron ;
    RecipVPSlashCN slash cn =
      ExtendPor.ReflRNP slash (ExtendPor.ReflPoss NumPl cn) ;

  lin FocusComp comp np = mkClause (comp.s ! np.a) np.hasClit np.isPol np.a (insertComplement (\\_ => (np.s ! Nom).ton) (predV (selectCopula comp.cop))) ;

} ;
