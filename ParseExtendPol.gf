concrete ParseExtendPol of ParseExtend =
 ExtendPol - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP],
 NumeralPol - [num], PunctuationX ** open Prelude, ResPol, VerbMorphoPol in {

lin
    UttAP  p ap = {s = ap.s ! AF p.gn Nom} ;
--    UttVPS p vps= {s = vps.s ! Indic ! p.a ! True} ;

    PhrUttMark pconj utt voc mark = {s = SOFT_BIND ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin
    UttVP ant pol p vp = {
        s = vp.prefix ++
            pol.s ++
            infinitive_form vp.verb vp.imienne pol.p p.gn ++ 
            vp.sufix ! pol.p ! MascPersSg
    };

lincat Sub1000000000 =
    { s:Case * Gender => Str; 
      o:AForm => Str;
      a:Accom; n:Number };

lin pot3as4 n = n ;

    num a = { s = \\x,y=>a.s!<x,y>; o=a.o; a=a.a; n=a.n };

lin BareN2 n = n ;

}
