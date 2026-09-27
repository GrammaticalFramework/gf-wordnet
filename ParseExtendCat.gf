concrete ParseExtendCat of ParseExtend =
  ExtendCat - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralCat - [num], PunctuationX **
  open Prelude, CommonRomance, ResCat, GrammarCat, ExtendCat in {

lin UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lincat CNN = {s1 : Bool => Str ; s2 : Str ; n1,n : Number ; g1 : Gender} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\d => num1.s ! cn1.g ++ cn1.s ! num1.n ;
      s2 = num2.s ! cn2.g ++ cn2.s ! num2.n ;
      n1 = num1.n ;
      g1 = cn1.g ;
      n = conjNumber num1.n num2.n
      } ;
    DetCNN quant conj cnn = heavyNPpol quant.isNeg {
      s = \\c => quant.s ! False ! cnn.n1 ! cnn.g1 ! c ++
                  conj.s1 ++ cnn.s1 ! False ++ conj.s2 ++ cnn.s2 ;
      a = agrP3 cnn.g1 cnn.n ;
      hasClit = False
      } ;
    ReflPossCNN conj cnn = {
      s = \\agr,c => possCase cnn.g1 cnn.n1 c ++ he_Pron.poss ! cnn.n1 ! cnn.g1 ++
                      conj.s1 ++ cnn.s1 ! False ++ conj.s2 ++ cnn.s2
      } ;
    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,c => quant.s ! False ! cnn.n1 ! cnn.g1 ! c ++
                      conj.s1 ++ cnn.s1 ! False ++ rnp.s ! agr ! genitive ++
                      conj.s2 ++ cnn.s2 ++ rnp.s ! agr ! genitive
      } ;

lin
    gen_Quant = DefArt ;

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

    ReflVPSlash = ExtendCat.ReflRNP ;
    ReflA2 a rnp = a ** {
      s = \\af => a.s ! af ++ a.c2.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! a.c2.c ;
      isPre = False
      } ;
    RecipVPSlash slash = insertRefl slash ;
    RecipVPSlashCN slash cn = insertRefl slash ;

lin
    NumLess n = {s = \\g => n.s ! g ++ "menys" ; n = n.n ; isNum = n.isNum} ;
    NumMore n = {s = \\g => n.s ! g ++ "més" ; n = n.n ; isNum = n.isNum} ;

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
      s = \\af => ap.s ! af ++ "prou" ++ ant.s ++ pol.s ++
                    infVP vp pol.p (aform2aagr af ** {p = P3})
      } ;
    EnoughAdv adv = adv ;
    TimeNP np = {s = (np.s ! Nom).ton} ;
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

    whatSgFem_IP = whatSg_IP ** {a = aagr Fem Sg} ;
    whatSgNeut_IP = whatSg_IP ;

    InOrderToVP ant pol p vp = {
      s = "per" ++ ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;

    FocusComp comp np = mkClause (comp.s ! np.a) np.hasClit np.isNeg np.a
      (insertComplement (\\_ => (np.s ! Nom).ton) (predV (selectCopula comp.cop))) ;

lin num x = x ;

lin RelNP = GrammarCat.RelNP ;
    ExtRelNP = GrammarCat.RelNP ;

lin BareN2 n = n ;

lin that_RP = IdRP ;

}
