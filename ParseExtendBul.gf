concrete ParseExtendBul of ParseExtend = ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Prelude, ResBul, NounBul in {

lin gen_Quant = DefArt ;

    UttAPFem ap = {s = ap.s ! ASg Fem Indef ! P3} ;
    UttVPS vps = {s = vps.s ! agrP3 (GSg Masc)} ;
    UttVPSFem vps = {s = vps.s ! agrP3 (GSg Fem)} ;

}
