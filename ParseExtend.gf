abstract ParseExtend = Extend - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
                                 CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP] ** {

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
    AdvRAP : AP -> Prep -> RNP -> AP ;
    PossPronRNP : Pron -> Num -> CN -> RNP -> NP ;
    ReflA2 : A2 -> RNP -> AP ;
    ReflVPSlash : VPSlash -> RNP -> VP ;

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

fun EmbedVP : Ant -> Pol -> VP -> SC ;
    ComplVV : VV -> Ant -> Pol -> VP -> VP ;
    SlashVV  : VV  -> Ant -> Pol -> VPSlash -> VPSlash ;
    SlashV2V : V2V -> Ant -> Pol -> VP -> VPSlash ;
    SlashV2VNP : V2V -> NP -> Ant -> Pol -> VPSlash -> VPSlash ;
    InOrderToVP : Ant -> Pol -> VP -> Adv ;
    CompVP : Ant -> Pol -> VP -> Comp ;
    UttVP : Ant -> Pol -> VP -> Utt ;
    UttVPMasc : Ant -> Pol -> VP -> Utt ;
    UttVPFem : Ant -> Pol -> VP -> Utt ;

}
