abstract ParseExtend = Extend - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
                                 CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP], Numeral - [num] ** {

cat Mark ;

fun gen_Quant : Quant ;

    UttAP     : AP -> Utt ;
    UttAPMasc : AP -> Utt ;
    UttAPFem  : AP -> Utt ;

    UttVPS     : VPS -> Utt ;
    UttVPSMasc : VPS -> Utt ;
    UttVPSFem  : VPS -> Utt ;
    UttVPSPl   : VPS -> Utt ;

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

fun NumLess : Num -> Num ;
    NumMore : Num -> Num ;

fun UseACard    : ACard -> Card ;
    UseAdAACard : AdA -> ACard -> Card ;

fun RelNP : NP -> RS -> NP ;
    ExtRelNP : NP -> RS -> NP ;

fun BareN2 : N2 -> N ;

fun ComparAdv : Pol -> CAdv -> Adv -> Comp -> Adv ;
    CAdvAP    : Pol -> CAdv -> AP  -> Comp -> AP ;

    AdnCAdv : Pol -> CAdv -> AdN ;

    EnoughAP  : AP -> Ant -> Pol -> VP -> AP ;
    EnoughAdv : Adv -> Adv ;
    ExtAdvAP : AP -> Adv -> AP ;

fun TimeNP : NP -> Adv ;

fun AdvAdv : Adv -> Adv -> Adv ;

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

fun RecipVPSlash   : VPSlash -> VP ;
    RecipVPSlashCN : VPSlash -> CN -> VP ;

cat Sub1000000000 ;

fun pot3as4 : Sub1000000 -> Sub1000000000 ;              -- coercion of 1..999999
    pot4  : Sub1000 -> Sub1000000000 ;                   -- m * 1000000000
    pot4plus : Sub1000 -> Sub1000000 -> Sub1000000000 ;  -- m * 1000000000 + n

    pot21 : Sub1000 ;                                    -- 100
    pot31 : Sub1000000 ;                                 -- 1000
    pot41 : Sub1000000000 ;                              -- 1000000000

    num : Sub1000000000 -> Numeral ;
}
