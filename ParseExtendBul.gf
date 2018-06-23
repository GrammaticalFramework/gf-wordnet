concrete ParseExtendBul of ParseExtend = ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP] ** open Predef, Prelude, ResBul, GrammarBul in {

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

lincat CNN = {s : Bool => Ints 3 => Species => Role => Str ; n1,n : NNumber ; g1 : AGender; nonEmpty : Bool} ;

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

lin RelNP    = GrammarBul.RelNP ;
    ExtRelNP np rs = {
      s = \\r => np.s ! r ++ comma ++ rs.s ! np.a ;
      a = np.a ;
      p = np.p
      } ;

lin BareN2 n2 = n2 ;

lin ComparAsAP a comp = {
      s = \\aform,p => let gn = case aform of {
                                  ASg g _       => GSg g ;
                                  ASgMascDefNom => GSg Masc ;
                                  APl   _       => GPl
                                }
                       in a.s ! aform ! p ++ "колкото" ++ comp.s ! agrP3 gn ;
      adv = a.adv ++ "колкото" ++ comp.s ! agrP3 (GSg Neut) ;
      isPre = True
    } ;

    ComparAsAdv adv comp = {
      s = adv.s ++ "колкото" ++ comp.s ! agrP3 (GSg Neut)
    } ;

    AdvAP_DAP ap prep dap =
      let adverb : AForm => Str 
                 = \\aform => let g = case aform of {
                                        ASg Masc _    => AMasc NonHuman ;
                                        ASg Fem _     => AFem ;
                                        ASg Neut _    => ANeut ;
                                        ASgMascDefNom => AMasc Human ;
                                        _             => ANeut
                                      } ;
                                  s = dap.s ! False ! g ! RObj prep.c
                              in prep.s ++ 
                                 case prep.c of {
                                   Dat      => "на" ++ s;
                                   WithPrep => with_Word ++ s;
                                   _        => s
                                 } ;
      in { s     = \\aform,p => ap.s ! aform ! p ++ adverb ! aform ;
           adv   = ap.adv ++ adverb ! ASg Neut Indef ;
           isPre = False
         } ;

lin AdvImp adv imp = {
      s = \\pol,gennum => adv.s ++ imp.s ! pol ! gennum
    } ;

lin whatSgFem_IP  = mkIP "каква" "каква" (GSg Fem) ;
    whatSgNeut_IP = mkIP "какво" "какво" (GSg Neut) ;

lin that_RP = IdRP ;

}
