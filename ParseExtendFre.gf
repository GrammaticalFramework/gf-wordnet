concrete ParseExtendFre of ParseExtend =
  ExtendFre - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralFre - [num], PunctuationX **
 open Prelude, CommonRomance, ResFre, GrammarFre, ExtendFre in {

lin
    UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps = {s = vps.s ! Indic ! p.a ! False} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat CNN = {
      s1 : Bool => Str ; s2 : Str ;
      refl1 : Agr => Str ; refl2 : Agr => Str ;
      firstNum : Number ; secondNum : Number ; n : Number ;
      firstGen : Gender ; secondGen : Gender
      } ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\_ => num1.s ! cn1.g ++ cn1.s ! num1.n ;
      s2 = num2.s ! cn2.g ++ cn2.s ! num2.n ;
      refl1 = \\agr => possDetPlain agr num1.n cn1.g ++ cn1.s ! num1.n ;
      refl2 = \\agr => possDetPlain agr num2.n cn2.g ++ cn2.s ! num2.n ;
      firstNum = num1.n ;
      secondNum = num2.n ;
      firstGen = cn1.g ;
      secondGen = cn2.g ;
      n = conjNumber num1.n num2.n
      } ;
    DetCNN quant conj cnn = heavyNPpol quant.isNeg {
      s = \\c => quant.s ! False ! cnn.firstNum ! cnn.firstGen ! c ++
                  cnn.s1 ! False ++ conj.s1 ++ conj.s2 ++
                  quant.s ! False ! cnn.secondNum ! cnn.secondGen ! c ++ cnn.s2 ;
      a = agrP3 cnn.firstGen cnn.n ;
      hasClit = False
      } ;
    ReflPossCNN conj cnn = {
      s = \\agr,c => prepCase c ++ conj.s1 ++ cnn.refl1 ! agr ++
                      conj.s2 ++ cnn.refl2 ! agr
      } ;
    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,c => quant.s ! False ! cnn.firstNum ! cnn.firstGen ! c ++
                      cnn.s1 ! False ++ rnp.s ! agr ! (CPrep P_de) ++
                      conj.s1 ++ conj.s2 ++ quant.s ! False ! cnn.secondNum ! cnn.secondGen ! c ++
                      cnn.s2 ++ rnp.s ! agr ! (CPrep P_de)
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
      insertComplement (\\a => prepCase vv.c2.c ++ ant.s ++ pol.s ++ vf a) (predV vv) ;
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

    ReflVPSlash = ExtendFre.ReflRNP ;
    ReflA2 a rnp = a ** {
      s = \\af => a.s ! af ++ a.c2.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! a.c2.c ;
      isPre = False
      } ;
    RecipVPSlash slash = insertRefl slash ;
    RecipVPSlashCN slash cn = insertRefl slash ;

lin
    NumLess n = {s = \\g => n.s ! g ++ "de moins" ; n = n.n ; isNum = n.isNum} ;
    NumMore n = {s = \\g => n.s ! g ++ "de plus" ; n = n.n ; isNum = n.isNum} ;

    UseACard ac = {s = \\_ => ac.s ; n = Pl} ;
    UseAdAACard ada ac = {s = \\_ => ada.s ++ ac.s ; n = Pl} ;

    ExtAdvAP ap adv = ap ** {s = \\af => ap.s ! af ++ bindComma ++ adv.s} ;

    ComparAdv pol cadv adv comp = let neg = (negation ! pol.p).p1 in {
      s = pol.s ++ neg ++ cadv.s ++ adv.s ++ comp.s ! Ag Masc Sg P3
      } ;
    CAdvAP pol cadv ap comp = let neg = (negation ! pol.p).p1 in ap ** {
      s = \\af => pol.s ++ neg ++ cadv.s ++ ap.s ! af ++ comp.s ! Ag Masc Sg P3
      } ;
    AdnCAdv pol cadv = let neg = (negation ! pol.p).p1 in {
      s = pol.s ++ neg ++ cadv.s ++ "que"
      } ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\af => "assez" ++ ap.s ! af ++ "pour" ++ ant.s ++ pol.s ++
                    infVP vp pol.p (aform2aagr af ** {p = P3})
      } ;
    EnoughAdv adv = adv ;
    TimeNP np = {s = (np.s ! Nom).ton} ;
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

    whatSgFem_IP = whatSg_IP ** {a = aagr Fem Sg} ;
    whatSgNeut_IP = whatSg_IP ;

    InOrderToVP ant pol p vp = {
      s = "afin de" ++ ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;

    FocusComp comp np = mkClause (comp.s ! np.a) np.hasClit np.isNeg np.a
      (insertComplement (\\_ => (np.s ! Nom).ton) (predV (selectCopula comp.cop))) ;

lin num x = x ;

lin that_RP = IdRP ;

lin RelNP = GrammarFre.RelNP ;
    ExtRelNP = GrammarFre.RelNP ;

lin BareN2 n = n ;

lin gen_Quant = {
      s = \\b,n,g,c => if_then_Str b (prepCase c) "" ;
      sp = \\n,g,c => artIndef True g n c ;
      spn= \\c => artIndef True Masc Sg c ;
      s2 = [] ;
      isNeg = False
      } ;

oper possDetPlain : Agr -> Number -> Gender -> Str ;
     possDetPlain = \agr,n,g -> case <agr.n,agr.p,n,g> of {
       <Sg,P1,Sg,Masc> => "mon" ; <Sg,P1,Sg,Fem> => "ma" ; <Sg,P1,Pl,_> => "mes" ;
       <Sg,P2,Sg,Masc> => "ton" ; <Sg,P2,Sg,Fem> => "ta" ; <Sg,P2,Pl,_> => "tes" ;
       <Sg,P3,Sg,Masc> => "son" ; <Sg,P3,Sg,Fem> => "sa" ; <Sg,P3,Pl,_> => "ses" ;
       <Pl,P1,Sg,_> => "notre" ; <Pl,P1,Pl,_> => "nos" ;
       <Pl,P2,Sg,_> => "votre" ; <Pl,P2,Pl,_> => "vos" ;
       <Pl,P3,Sg,_> => "leur" ; <Pl,P3,Pl,_> => "leurs"
       } ;

}
