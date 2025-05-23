concrete ParseExtendIta of ParseExtend =
  ExtendIta - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralIta - [num], PunctuationX **
  open Prelude, CommonRomance, ResIta, GrammarIta, ExtendIta in {

lin
    UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;
    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

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
    CompVP ant pol p vp = {
        s = \\agr => ant.s ++ pol.s ++ "de" ++ infVP vp pol.p p.a ;
        cop = serCopula
      } ;
    UttVP ant pol p vp = {
        s = ant.s ++ pol.s ++ infVP vp pol.p p.a
      } ;

    ReflVPSlash = ExtendIta.ReflRNP ;

lin num x = x ;

lin RelNP = GrammarIta.RelNP ;
    ExtRelNP = GrammarIta.RelNP ;

lin BareN2 n = n ;


}
