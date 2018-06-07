concrete ParseExtendSwe of ParseExtend = ExtendSwe - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Prelude, ResSwe, CommonScand, NounSwe, PhraseSwe in {

lincat Mark = {s : Str} ;

lin gen_Quant = DefArt ;

    UttAPFem = UttAP ;
    UttVPS vps = {s = vps.s ! Main ! (agrP3 Utr Sg)} ;
    UttVPSFem vps = {s = vps.s ! Main ! (agrP3 Utr Sg)} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

    AdvRNP np prep rnp = {s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a; isPron = False} ;
    AdvRVP vp prep rnp = insertObjPost (\\a => prep.s ++ rnp.s ! a) vp ;
    
    AdvImp adv imp = {
      s = \\pol,num => adv.s ++ imp.s ! pol ! num
    } ;

}
