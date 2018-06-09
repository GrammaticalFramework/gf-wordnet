concrete ParseExtendBul of ParseExtend = ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Predef, Prelude, ResBul, NounBul, IdiomBul in {

lincat Mark = {s : Str} ;

lin gen_Quant = DefArt ;

    UttAPFem ap = {s = ap.s ! ASg Fem Indef ! P3} ;
    UttVPS vps = {s = vps.s ! agrP3 (GSg Masc)} ;
    UttVPSFem vps = {s = vps.s ! agrP3 (GSg Fem)} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

    AdvRNP np prep rnp = {s = \\role => np.s ! role ++ prep.s ++ rnp.s ! RObj prep.c; a = np.a; p = np.p} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! RObj prep.c) Pos vp ;
    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = rnp.s; a = rnp.a; p=rnp.p})) ;    

lincat CNN = {s : Bool => Ints 2 => Species => Role => Str ; n1,n : NNumber ; g1 : AGender; nonEmpty : Bool} ;

lin BaseCNN num1 cn1 num2 cn2 = 
      let mknf : NNumber -> Species -> Role -> AGender -> NForm =
            \nn,spec,role,g ->
                case <nn,spec> of {
                  <NNum Sg,Def>   => case role of {
                                       RSubj => NFSgDefNom ;
                                       RVoc  => NFVocative ;
                                       _     => NF Sg Def
                                     } ;
                  <NNum Sg,Indef> => case role of {
				                       RVoc  => NFVocative ;
				                       _     => NF Sg Indef
                                     } ;
                  <NNum Pl,Def>   => NF Pl Def ;
                  <NNum Pl,Indef> => NF Pl Indef;
                  <NCountable,Def>   => NF Pl spec ;
                  <NCountable,Indef> => case g of {
                                          AMasc Human => NF Pl Indef;
                                          _           => NFPlCount
                                        }
                } ;
      in { s  = \\d,t,spec,role => num1.s ! dgenderSpecies cn1.g spec role ++ cn1.s ! mknf num1.nn spec role cn1.g ++ linCoord!t ++ num2.s ! dgenderSpecies cn2.g spec role ++ cn2.s ! mknf num2.nn spec role cn2.g ;
           n1 = num1.nn ;
           n  = num1.nn ;
           g1 = cn1.g ;
           nonEmpty = num1.nonEmpty
      } ;
      
    DetCNN quant conj cnn =
      { s = \\role => 
                let spec = case cnn.nonEmpty of {True=>Indef; _=>quant.spec} ;
                    s    = quant.s ! True ! aform (gennum cnn.g1 (numnnum cnn.n)) (case role of {RVoc=>Indef; _=>Def}) role ++ 
                           conj.s ++ (linCoordSep [])!conj.distr!conj.conj ++ 
                           cnn.s ! conj.distr ! conj.conj ! spec ! role
                in case role of {
                     RObj Dat      => "на" ++ s;
                     RObj WithPrep => case quant.p of {
                                        Pos => with_Word ++ s ;
                                        Neg => "без" ++ s
                                      } ;
                     _             => s
                   } ;
        a = {gn = gennum cnn.g1 (numnnum cnn.n); p = P3} ;
        p = quant.p ;
        g = cnn.g1
      } ;
      
    ReflPossCNN conj cnn = {
        s = \\role => reflPron ! aform (gennum cnn.g1 (numnnum cnn.n1)) Def (RObj Acc) ++
                      conj.s ++ (linCoordSep [])!conj.distr!conj.conj ++ 
                      cnn.s ! conj.distr ! conj.conj ! Def ! role ;
        a = {gn = gennum cnn.g1 (numnnum cnn.n); p = P3} ;
        p = Pos
      } ;
      
    PossCNN_RNP quant conj cnn rnp =
      { s = \\role =>
                let spec = case cnn.nonEmpty of {True=>Indef; _=>quant.spec} ;
                    s    = quant.s ! True ! aform (gennum cnn.g1 (numnnum cnn.n)) (case role of {RVoc=>Indef; _=>Def}) role ++ 
                           conj.s ++ (linCoordSep [])!conj.distr!conj.conj ++ 
                           cnn.s ! conj.distr ! conj.conj ! spec ! role ++
                           "на" ++ rnp.s ! (RObj Acc)
                in case role of {
                     RObj Dat      => "на" ++ s;
                     RObj WithPrep => case quant.p of {
                                        Pos => with_Word ++ s ;
                                        Neg => "без" ++ s
                                      } ;
                     _             => s
                   } ;
        a = {gn = gennum cnn.g1 (numnnum cnn.n); p = P3} ;
        p = quant.p ;
        g = cnn.g1
      } ;

lin ComparAsAP a np = {
      s = \\aform,_ => a.s ! aform ++ "като" ++ np.s ! RObj Acc ;
      adv = a.adv ++ "като" ++ np.s ! RObj Acc ;
      isPre = True
    } ;

lin AdvImp adv imp = {
      s = \\pol,gennum => adv.s ++ imp.s ! pol ! gennum
    } ;
    
lin ImperfectVP vp = ProgrVP vp ;

}
