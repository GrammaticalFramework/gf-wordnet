concrete ParseExtendFre of ParseExtend =
  ExtendFre - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralFre - [num], PunctuationX **
 open Prelude, CommonRomance, ResFre, GrammarFre in {

lin
    UttAP  p ap = {s = ap.s ! (genNum2Aform p.a.g p.a.n)} ;

    PhrUttMark pconj utt voc mark = {s = SOFT_BIND ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

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

lin num x = x ;

lin RelNP = GrammarFre.RelNP ;
    ExtRelNP = GrammarFre.RelNP ;

lin BareN2 n = n ;

}
