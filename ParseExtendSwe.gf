concrete ParseExtendSwe of ParseExtend = 
  ExtendSwe - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP] ** 
  open Prelude, ResSwe, CommonScand, GrammarSwe, Coordination in {

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
    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = \\_ => rnp.s ! pron.a; a = pron.a; isPron=False})) ;

lin FocusComp comp np = mkClause (comp.s ! np.a) np.a (insertObj (\\_ => np.s ! nominative) (predV verbBe)) ;

lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp x y = twoTable Agr x y ;
    ConsComp xs x = consrTable Agr comma xs x ;
    ConjComp conj ss = conjunctDistrTable Agr conj ss ;

lincat CNN = {s1,s2 : DetSpecies => Case => Str ; n1,n : Number ; g1 : NGender ; isMod,isDet : Bool} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\spec,c => num1.s ! cn1.g ++ cn1.s ! num1.n ! spec ! c ;
      s2 = \\spec,c => num2.s ! cn2.g ++ cn2.s ! num2.n ! spec ! c ;
      n1 = num1.n ;
      n  = conjNumber num1.n num2.n ;
      g1 = cn1.g ;
      isMod = cn1.isMod ;
      isDet = num1.isDet
    } ;

    DetCNN quant conj cnn =
      let md : Bool -> Bool = \b -> 
            case quant.det of {
              DDef _ => orB b cnn.isDet ;
              DIndef => cnn.isDet
            } ;
          dd = case <quant.det,detDef,cnn.isMod> of {
                 <DDef Def, Indef, True> => DDef Indef ;
                 <d,_,_> => d
               }
      in {
        s = \\c => quant.s ! cnn.n1 ! cnn.isMod ! md cnn.isMod ! cnn.g1 ++ conj.s1 ++ cnn.s1 ! dd ! caseNP c ++ conj.s2 ++ cnn.s2 ! dd ! caseNP c ;
        a = agrP3 (ngen2gen cnn.g1) (conjNumber conj.n cnn.n) ;
        isPron = False
      } ;

    ReflPossCNN conj cnn = {
      s = \\a => possPron a.n a.p cnn.n (ngen2gen cnn.g1) ++ conj.s1 ++ cnn.s1 ! DDef Indef ! Nom ++ conj.s2 ++ cnn.s2 ! DDef Indef ! Nom ;
      isPron = False
    } ;

    PossCNN_RNP quant conj cnn rnp =
      let md : Bool -> Bool = \b -> 
            case quant.det of {
              DDef _ => orB b cnn.isDet ;
              DIndef => cnn.isDet
            } ;
          dd = case <quant.det,detDef,cnn.isMod> of {
                 <DDef Def, Indef, True> => DDef Indef ;
                 <d,_,_> => d
               }
      in {
        s = \\a => quant.s ! cnn.n1 ! cnn.isMod ! md cnn.isMod ! cnn.g1 ++ conj.s1 ++ cnn.s1 ! dd ! Nom ++ conj.s2 ++ cnn.s2 ! dd ! Nom ++ av_Prep ++ rnp.s ! a;
        isPron = False
      } ;

lin RelNP np rs = {
      s = \\c => np.s ! c ++ rs.s ! np.a ! RNom ;
      a = np.a ;
      isPron = False
      } ;
    ExtRelNP = GrammarSwe.RelNP ;

lin BareN2 n2 = n2 ;

lin ComparAsAP a comp = {
      s = \\ap => let agr = case ap of {
                              Strong (GSg g) => agrP3 g Sg;
                              Strong GPl     => agrP3 Neutr Pl;
                              Weak n         => agrP3 Neutr n
                            }
                  in a.s ! ap ++ "som" ++ comp.s ! agr ;
      isPre = False
    } ;

lin ComparAsAdv adv comp = {
      s = adv.s ++ "som" ++ comp.s ! agrP3 Neutr Sg
    } ;

lin UseDAP dap = 
      let 
        g = neutrum ; ----
        m = True ;  ---- is this needed for other than Art?
      in {
        s = \\c => dap.sp ! m ! g ;    ---- case of det!
        a = agrP3 (ngen2gen g) dap.n ;
        isPron = False
      } ;

lin UseDAPMasc, UseDAPFem = \dap ->
      let 
        g = utrum ; ----
        m = True ;  ---- is this needed for other than Art?
      in {
        s = \\c => dap.sp ! m ! g ;    ---- case of det!
        a = agrP3 (ngen2gen g) dap.n ;
        isPron = False
      } ;

lin AdvImp adv imp = {
      s = \\pol,num => adv.s ++ imp.s ! pol ! num
    } ;

lin whatSgFem_IP, whatSgNeut_IP = whatSg_IP ;

lin that_RP = IdRP ;

}
