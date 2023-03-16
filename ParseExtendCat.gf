concrete ParseExtendCat of ParseExtend =
  ExtendCat - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralCat - [num], PunctuationX **
  open Prelude, CommonRomance, ResCat, GrammarCat in {

lin UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin
    EmbedVP ant pol p vp = {
        s = \\c => prepCase c ++ ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;
    CompVP ant pol p vp = {
        s = \\agr => ant.s ++ pol.s ++ "de" ++ infVP vp pol.p p.a ;
        cop = serCopula
      } ;
    UttVP ant pol p vp = {
        s = ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;

lin num x = x ;

lin RelNP = GrammarCat.RelNP ;
    ExtRelNP = GrammarCat.RelNP ;

lin BareN2 n = n ;


}
