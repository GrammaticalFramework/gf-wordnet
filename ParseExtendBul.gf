concrete ParseExtendBul of ParseExtend = ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Prelude, ResBul, NounBul in {

lincat Mark = {s : Str} ;

lin gen_Quant = DefArt ;

    UttAPFem ap = {s = ap.s ! ASg Fem Indef ! P3} ;
    UttVPS vps = {s = vps.s ! agrP3 (GSg Masc)} ;
    UttVPSFem vps = {s = vps.s ! agrP3 (GSg Fem)} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

    AdvRNP np prep rnp = {s = \\role => np.s ! role ++ prep.s ++ rnp.s ! RObj prep.c; a = np.a; p = np.p} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! RObj prep.c) Pos vp ;

    AdvImp adv imp = {
      s = \\pol,gennum => adv.s ++ imp.s ! pol ! gennum
    } ;
}
