concrete ParseExtendEng of ParseExtend = ExtendEng - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Prelude, ResEng, PhraseEng in {

lincat Mark = {s : Str} ;

lin gen_Quant = {
      s  = \\hasCard,n => "" ;
      sp = \\hasCard,n => case <n,hasCard> of {
        <Sg,False> => table { NCase Gen => "its"; _ => "it" } ;
        <Pl,False> => table { NCase Nom => "they"; NPAcc => "them"; _ => "theirs" } ;
        _          => \\c => artDef
        }
      } ;

    UttAPFem = UttAP ;
    UttVPS vps = {s = vps.s ! agrP3 Sg}  ;
    UttVPSFem vps = {s = vps.s ! agrP3 Sg}  ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

}
