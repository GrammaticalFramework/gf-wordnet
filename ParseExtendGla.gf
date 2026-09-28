concrete ParseExtendGla of ParseExtend =
  ExtendGla - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron,
               GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, UncontractedNeg,
               AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralGla - [num],
  PunctuationX **
  open Prelude, ResGla in {

  lincat CNN = {s : Case => Str ; n : Number} ;

  lin
    gen_Quant = {s = \\_ => [] ; sp = [] ; qt = QDef Indef} ;
    UttAP pron ap = {s = ap.s ! ASg NOM Masc} ;
    UttVPS pron vps = {s = vps.s ++ pron.s ! NOM} ;
    PhrUttMark pc u voc mark = {s = pc.s ++ u.s ++ voc.s ++ mark.s} ;
    BareN2 n = n ;
    RelNP np rs = np ** {s = \\c => np.s ! c ++ rs.s} ;
    ExtRelNP np rs = np ** {s = \\c => np.s ! c ++ "," ++ rs.s} ;
    ExtAdvAP ap adv = addAPX ap ("," ++ adv.s) ;
    BaseCNN n1 c1 n2 c2 = {s = \\c => c1.s ! c ! Indef ! n1.n ++ c2.s ! c ! Indef ! n2.n ; n = Pl} ;
    DetCNN q conj cnn = emptyNP ** {
      art = \\c => q.s ! getQuantForm cnn.n Masc c ; s = cnn.s ;
      voc = cnn.s ! NOM ; a = NotPron (DDef cnn.n Def)
      } ;
    ReflPossCNN conj cnn = lin NP (emptyNP ** {
      s = cnn.s ; voc = cnn.s ! NOM ; a = NotPron (DDef cnn.n Def)
      }) ;
    PossCNN_RNP q conj cnn rnp = lin NP (emptyNP ** {
      art = \\c => q.s ! getQuantForm cnn.n Masc c ;
      s = \\c => cnn.s ! c ++ rnp.s ! c ; voc = cnn.s ! NOM ++ rnp.voc ;
      a = NotPron (DDef cnn.n Def)
      }) ;
    ReflVPSlash slash rnp = appendVP slash (rnp.s ! NOM) ;
    ReflA2 a rnp = addAPX a (prepNP a.c2 rnp) ;
    NumLess n = n ** {s = n.s ++ "nas lugha"} ;
    NumMore n = n ** {s = n.s ++ "a bharrachd"} ;
    UseACard c = {s = c.s ; n = Pl} ;
    UseAdAACard a c = {s = a.s ++ c.s ; n = Pl} ;
    ComparAdv pol ca adv comp = {s = pol.s ++ ca.s ++ adv.s ++ ca.p ++ comp.s} ;
    CAdvAP pol ca ap comp = addAPX ap (ca.p ++ comp.s) ;
    AdnCAdv pol ca = {s = pol.s ++ ca.s} ;
    EnoughAP ap ant pol vp = addAPX ap ("gu leòr airson" ++ vp.s) ;
    EnoughAdv adv = {s = adv.s ++ "gu leòr"} ;
    TimeNP np = {s = linNP np} ;
    AdvAdv a b = {s = a.s ++ b.s} ;
    whatSgFem_IP = {s = "dè"} ;
    whatSgNeut_IP = {s = "dè"} ;
    that_RP = {s = "a"} ;
    EmbedVP ant pol pron vp = {s = pron.s ! NOM ++ pol.s ++ "a" ++ vp.noun} ;
    ComplVV vv ant pol vp = appendVP vv (pol.s ++ "a" ++ vp.noun) ;
    SlashVV vv ant pol slash = appendSlash (vv ** {c2 = slash.c2}) (pol.s ++ "a" ++ slash.noun) ;
    SlashV2V v ant pol vp = appendSlash v (pol.s ++ "a" ++ vp.noun) ;
    SlashV2VNP v np ant pol slash = appendSlash v (linNP np ++ pol.s ++ "a" ++ slash.noun) ;
    InOrderToVP ant pol pron vp = {s = "gus" ++ pron.s ! NOM ++ pol.s ++ vp.noun} ;
    CompVP ant pol pron vp = {s = pron.s ! NOM ++ pol.s ++ "a" ++ vp.noun} ;
    UttVP ant pol pron vp = {s = pron.s ! NOM ++ pol.s ++ "a" ++ vp.noun} ;
    RecipVPSlash slash = appendVP slash "a chèile" ;
    RecipVPSlashCN slash cn = appendVP slash (cn.s ! NOM ! Indef ! Pl ++ "a chèile") ;
    FocusComp comp np = {subj = linNP np ; n = agrNumber np.a ; pred = appendVP parseBiV comp.s} ;
    num n = n ;

oper
  addAPX : LinAP -> Str -> LinAP = \ap,x -> {s = \\f => ap.s ! f ++ x ; voc = \\g => ap.voc ! g ++ x} ;
  parseBiV : LinV = {
    s = "bi" ; conditional = table {Sg => "bhiodh" ; Pl => "bhiodh"} ;
    imperative = table {P1 => table {Sg => "bitheam" ; Pl => "bitheamaid"} ; P2 => table {Sg => "bi" ; Pl => "bithibh"} ; P3 => table {Sg => "bitheadh" ; Pl => "bitheadh"}} ;
    future = table {Indep => "bidh" ; Dep => "bi"} ; past = table {Indep => "bha" ; Dep => "robh"} ; noun = "bhith" ; participle = "air a bhith"
    ; copular = True ; complement = []
    } ;
}
