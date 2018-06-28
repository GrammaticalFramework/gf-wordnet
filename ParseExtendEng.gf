concrete ParseExtendEng of ParseExtend = 
  ExtendEng - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP] ** 
  open Prelude, ResEng, GrammarEng, (E = ExtraEng), Coordination in {

lincat Mark = {s : Str} ;

lin gen_Quant = {
      s  = \\hasCard,n => "" ;
      sp = \\g,hasCard,n => case <n,hasCard> of {
        <Sg,False> => table { NCase Gen => table Gender ["its"; "his"; "her"] ! g; _ => table Gender ["it"; "he"; "she"] ! g } ;
        <Pl,False> => table { NCase Nom => "they"; NPAcc => "them"; _ => "theirs" } ;
        _          => \\c => artDef
        }
      } ;

    UttAPFem = UttAP ;
    UttVPS vps = {s = vps.s ! agrP3 Sg}  ;
    UttVPSFem vps = {s = vps.s ! agrP3 Sg}  ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

    AdvRNP np prep rnp = {s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! a) vp ;
    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = \\_ => rnp.s ! pron.a; a = pron.a})) ;

lin FocusComp comp np = mkClause (comp.s ! np.a) np.a (insertObj (\\_ => np.s ! npNom) (predAux auxBe)) ;

lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp x y = twoTable Agr x y ;
    ConsComp xs x = consrTable Agr comma xs x ;
    ConjComp conj ss = conjunctDistrTable Agr conj ss ;

lincat CNN = {s1,s2 : Case => Str; n1,n : Number; g1 : Gender; hasCard : Bool} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\c => num1.s ! Nom ++ cn1.s ! num1.n ! c ;
      s2 = \\c => num2.s ! Nom ++ cn2.s ! num2.n ! c ;
      n1 = num1.n ;
      g1 = cn1.g ;
      n  = conjNumber num1.n num2.n ;
      hasCard = num1.hasCard
    } ;

    DetCNN quant conj cnn = {
      s = \\c => quant.s ! cnn.hasCard ! cnn.n1 ++ conj.s1 ++ cnn.s1 ! npcase2case c ++ conj.s2 ++ cnn.s2 ! npcase2case c ;
      a = conjAgr (agrP3 conj.n) (agrgP3 cnn.n cnn.g1)
    } ;

    ReflPossCNN conj cnn = {s = \\a => possPron ! a ++ conj.s1 ++ cnn.s1 ! Nom ++ conj.s2 ++ cnn.s2 ! Nom} ;
    
    PossCNN_RNP quant conj cnn rnp = {
      s = \\a => quant.s ! cnn.hasCard ! cnn.n1 ++ conj.s1 ++ cnn.s1 ! Nom ++ conj.s2 ++ cnn.s2 ! Nom ++ "of" ++ rnp.s ! a ;
    } ;

lin RelNP np rs = {
      s = \\c => np.s ! c ++ rs.s ! np.a ++ finalComma ;
      a = np.a
      } ;
    ExtRelNP = GrammarEng.RelNP ;

lin BareN2 n2 = n2 ;

lin ComparAsAP a comp = {
      s = \\agr => "as" ++ a.s ! agr ++ "as" ++ comp.s ! agr ;
      isPre = False
    } ;

lin ComparAsAdv adv comp = {
      s = "as" ++ adv.s ++ "as" ++ comp.s ! agrP3 Sg
    } ;

lin UseDAP dap = {
      s = dap.sp ! Neutr ! False ;
      a = agrP3 dap.n
    } ;

lin UseDAPMasc dap = {
      s = dap.sp ! Masc ! False ;
      a = agrgP3 dap.n Masc
    } ;

lin UseDAPFem dap = {
      s = dap.sp ! Fem ! False ;
      a = agrgP3 dap.n Fem
    } ;

lin AdvImp adv imp = {
      s = \\pol,impform => adv.s ++ imp.s ! pol ! impform
    } ;

lin whatSgFem_IP, whatSgNeut_IP = whatSg_IP ;

lin that_RP = E.that_RP ;

}
