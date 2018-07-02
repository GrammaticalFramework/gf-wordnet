concrete ParseExtendBul of ParseExtend = 
  ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP] ** open Predef, Prelude, ResBul, GrammarBul in {

lincat Mark = {s : Str} ;

lin gen_Quant = DefArt ;

    UttAP     ap = {s = ap.s ! ASg Neut Indef ! P3} ;
    UttAPMasc ap = {s = ap.s ! ASg Masc Indef ! P3} ;
    UttAPFem  ap = {s = ap.s ! ASg Fem  Indef ! P3} ;

    UttVPS vps = {s = vps.s ! agrP3 (GSg Masc)} ;
    UttVPSFem vps = {s = vps.s ! agrP3 (GSg Fem)} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

    AdvRNP np prep rnp = {s = \\role => np.s ! role ++ prep.s ++ rnp.s ! RObj prep.c; a = np.a; p = np.p} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! RObj prep.c) Pos vp ;
    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = rnp.s; a = rnp.a; p=rnp.p})) ;    

lin FocusComp comp np =
      mkClause (comp.s ! np.a) np.a comp.p
               (insertObj (\\_ => np.s ! RSubj) np.p (predV verbBe)) ;

lincat [Comp] = {s : Bool => Ints 3 => Agr => Str} ;
lin BaseComp x y =
      {s = \\d,t,agr=>x.s!agr++linCoord!t++y.s!agr} ;
    ConsComp x xs =
      {s = \\d,t,agr=>x.s!agr++(linCoordSep bindComma)!d!t++xs.s!d!t!agr} ;
    ConjComp conj ss = {
      s = \\agr => conj.s ++ (linCoordSep [])!conj.distr!conj.conj++ss.s!conj.distr!conj.conj!agr;
      p = Pos
      } ;

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
      s = \\r => np.s ! r ++ bindComma ++ rs.s ! np.a ;
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

lin TimeNP np = {s = np.s ! RObj Acc} ;

lin UseDAP dap = {
      s = \\role => let s = dap.s ! False ! ANeut ! role
                    in case role of {
                         RObj Dat      => "на" ++ s;
                         RObj WithPrep => with_Word ++ s;
                         _             => s
                       } ;
      a = {gn = gennum ANeut (numnnum dap.nn); p = P3} ;
      p = dap.p
      } ;

    UseDAPMasc dap = {
      s = \\role => let s = dap.s ! False ! (AMasc Human) ! role
                    in case role of {
                         RObj Dat      => "на" ++ s;
                         RObj WithPrep => with_Word ++ s;
                         _             => s
                       } ;
      a = {gn = gennum (AMasc Human) (numnnum dap.nn); p = P3} ;
      p = dap.p
      } ;

    UseDAPFem dap = {
      s = \\role => let s = dap.s ! False ! AFem ! role
                    in case role of {
                         RObj Dat      => "на" ++ s;
                         RObj WithPrep => with_Word ++ s;
                         _             => s
                       } ;
      a = {gn = gennum AFem (numnnum dap.nn); p = P3} ;
      p = dap.p
      } ;

lin AdvImp adv imp = {
      s = \\pol,gennum => adv.s ++ imp.s ! pol ! gennum
    } ;

lin whatSgFem_IP  = mkIP "каква" "каква" (GSg Fem) ;
    whatSgNeut_IP = mkIP "какво" "какво" (GSg Neut) ;

lin that_RP = IdRP ;

lin EmbedVP ant pol vp = {s = \\agr => ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! agr} ;

    ComplVV vv ant pol vp =
      insertObj (\\agr => ant.s ++ pol.s ++ 
                          case vv.typ of {
                            VVInf asp => daComplex ant.a pol.p vp ! asp ! agr;
                            VVGerund  => gerund vp ! Imperf ! agr
                          }) vp.p
                (predV vv) ;

    SlashVV vv ant pol slash = {
      s = vv.s ;
      ad = {isEmpty=True; s=[]};
      compl1 = \\agr => ant.s ++ pol.s ++ daComplex ant.a pol.p {s=slash.s; ad=slash.ad; compl=slash.compl1; vtype=slash.vtype; p = Pos; isSimple = slash.isSimple} ! Perf ! agr ;
      compl2 = slash.compl2 ;
      vtype  = vv.vtype ;
      p  = slash.p ;
      c2 = slash.c2 ;
      isSimple = False ;
      subjCtrl = slash.subjCtrl
      } ;

    SlashV2V vv ant pol vp =
      insertSlashObj2 (\\agr => ant.s ++ pol.s ++ vv.c3.s ++ daComplex ant.a (orPol pol.p vp.p) vp ! Perf ! agr) Pos (slashV vv vv.c2 vv.subjCtrl) ;

    SlashV2VNP vv np ant pol slash = {
      s = vv.s ;
      ad = {isEmpty=True; s=[]};
      compl1 = \\agr => ant.s ++ pol.s ++ vv.c2.s ++ np.s ! RObj vv.c2.c ++ 
                        daComplex ant.a (orPol pol.p np.p) {s=slash.s; ad=slash.ad; compl=slash.compl1; vtype=slash.vtype; p=Pos; isSimple = slash.isSimple} ! Perf ! np.a ;
      compl2 = slash.compl2 ;
      vtype = vv.vtype ;
      p  = Pos ;
      c2 = slash.c2 ;
      isSimple = False ;
      subjCtrl = slash.subjCtrl
      } ;

    InOrderToVP ant pol vp =
      {s = "за" ++ ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! {gn=GSg Neut; p=P3}};

    CompVP ant pol vp = {s = \\agr => ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! agr; p = Pos} ;

    UttVP ant pol vp = {s = ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! agrP3 (GSg Neut)} ;
    UttVPMasc ant pol vp = {s = ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! agrP3 (GSg Masc)} ;
    UttVPFem  ant pol vp = {s = ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! agrP3 (GSg Fem)} ;

    ReflA2 a rnp = {
      s = \\aform,_ => a.s ! aform ++ a.c2 ++ rnp.s ! RObj Acc ;
      adv = a.adv ++ a.c2 ++ rnp.s ! RObj Acc ; 
      isPre = False
      } ;

    ReflVPSlash slash rnp = {
      s   = slash.s ;
      ad  = slash.ad ;
      compl = \\a => slash.compl1 ! a ++ slash.c2.s ++ rnp.s ! RObj slash.c2.c ++ slash.compl2 ! rnp.a ;
      vtype = slash.vtype ;
      p   = orPol rnp.p slash.p ;
      isSimple = False
    } ;
}
