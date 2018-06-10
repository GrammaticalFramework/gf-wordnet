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
    PossPronRNP : Pron -> Num -> CN -> RNP -> NP ;

cat CNN ;
fun BaseCNN : Num -> CN -> Num -> CN -> CNN ;
    DetCNN  : Quant -> Conj -> CNN -> NP ;

    ReflPossCNN : Conj -> CNN -> RNP ;
    PossCNN_RNP : Quant -> Conj -> CNN -> RNP -> RNP ;

fun BareN2 : N2 -> N ;

fun ComparAsAP : A -> NP -> AP ;

fun AdvImp : Adv -> Imp -> Imp ;

fun whatSgFem_IP : IP ;
    whatSgNeut_IP : IP ;

}
