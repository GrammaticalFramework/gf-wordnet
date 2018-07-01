abstract ParseExtend = Extend - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP] ** {

cat Mark ;

fun gen_Quant : Quant ;

    UttAP     : AP -> Utt ;
    UttAPMasc : AP -> Utt ;
    UttAPFem  : AP -> Utt ;

    UttVPS : VPS -> Utt ;
    UttVPSFem : VPS -> Utt ;

    PhrUttMark : PConj -> Utt -> Voc -> Mark -> Phr ;
    
    FullStop, ExclMark, QuestMark : Mark ;

    AdvRNP : NP -> Prep -> RNP -> RNP ;
    AdvRVP : VP -> Prep -> RNP -> VP ;
    PossPronRNP : Pron -> Num -> CN -> RNP -> NP ;

fun FocusComp : Comp -> NP -> Cl ;

cat [Comp]{2} ;
fun ConjComp : Conj -> ListComp -> Comp ;

cat CNN ;
fun BaseCNN : Num -> CN -> Num -> CN -> CNN ;
    DetCNN  : Quant -> Conj -> CNN -> NP ;

    ReflPossCNN : Conj -> CNN -> RNP ;
    PossCNN_RNP : Quant -> Conj -> CNN -> RNP -> RNP ;

fun RelNP : NP -> RS -> NP ;
    ExtRelNP : NP -> RS -> NP ;

fun BareN2 : N2 -> N ;

fun ComparAsAP : AP -> Comp -> AP ;
    ComparAsAdv : Adv -> Comp -> Adv ;

fun TimeNP : NP -> Adv ;

fun UseDAP     : DAP -> NP ;
    UseDAPMasc : DAP -> NP ;
    UseDAPFem  : DAP -> NP ;

fun AdvImp : Adv -> Imp -> Imp ;

fun whatSgFem_IP : IP ;
    whatSgNeut_IP : IP ;

fun that_RP : RP ;

}
