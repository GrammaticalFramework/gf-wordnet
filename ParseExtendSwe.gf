concrete ParseExtendSwe of ParseExtend = ExtendSwe - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Prelude, ResSwe, CommonScand, NounSwe, PhraseSwe in {

lin gen_Quant = DefArt ;

    UttAPFem = UttAP ;
    UttVPS vps = {s = vps.s ! Main ! (agrP3 Utr Sg)} ;
    UttVPSFem vps = {s = vps.s ! Main ! (agrP3 Utr Sg)} ;

}
