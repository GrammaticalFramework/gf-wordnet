abstract ParseExtend = Extend - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** {

cat Mark ;

fun gen_Quant : Quant ;

    UttAPFem : AP -> Utt ;
    
    UttVPS : VPS -> Utt ;
    UttVPSFem : VPS -> Utt ;
    
    PhrUttMark : PConj -> Utt -> Voc -> Mark -> Phr ;
    
    FullStop, ExclMark, QuestMark : Mark ;

    AdvRNP : NP -> Prep -> RNP -> RNP ;
    AdvRVP : VP -> Prep -> RNP -> VP ;

    AdvImp : Adv -> Imp -> Imp ;

}
