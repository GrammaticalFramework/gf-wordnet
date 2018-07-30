concrete ParseExtendBul of ParseExtend = 
  ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP], NumeralBul - [num] ** open Predef, Prelude, ResBul, GrammarBul, ParadigmsBul in {

lincat Mark = {s : Str} ;

lin gen_Quant = DefArt ;

    UttAP     ap = {s = ap.s ! ASg Neut Indef ! P3} ;
    UttAPMasc ap = {s = ap.s ! ASg Masc Indef ! P3} ;
    UttAPFem  ap = {s = ap.s ! ASg Fem  Indef ! P3} ;

    UttVPS     vps = {s = vps.s ! agrP3 (GSg Neut)} ;
    UttVPSMasc vps = {s = vps.s ! agrP3 (GSg Masc)} ;
    UttVPSFem  vps = {s = vps.s ! agrP3 (GSg Fem)} ;
    UttVPSPl   vps = {s = vps.s ! agrP3 GPl} ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

    AdvRNP np prep rnp = {s = \\role => np.s ! role ++ prep.s ++ rnp.s ! RObj prep.c; gn = np.gn; p = np.p} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! RObj prep.c) Pos vp ;
    AdvRAP ap prep rnp = {
      s = \\aform,p => ap.s ! aform ! p ++ prep.s ++ rnp.s ! RObj prep.c ;
      isPre = False
    } ;

    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = rnp.s; gn = rnp.gn; p=NounP3 Pos})) ;    

lin FocusComp comp np =
      mkClause (comp.s ! personAgr np.gn np.p) np.gn (NounP3 comp.p)
               (insertObj (\\_ => np.s ! RSubj) (personPol np.p) (predV verbBe)) ;

lincat [Comp] = {s : Bool => Ints 4 => Agr => Str} ;
lin BaseComp x y =
      {s = \\d,t,agr=>x.s!agr++linCoord!t++y.s!agr} ;
    ConsComp x xs =
      {s = \\d,t,agr=>x.s!agr++(linCoordSep bindComma)!d!t++xs.s!d!t!agr} ;
    ConjComp conj ss = {
      s = \\agr => conj.s ++ (linCoordSep [])!conj.distr!conj.conj++ss.s!conj.distr!conj.conj!agr;
      p = Pos
      } ;

lincat CNN = {s : Bool => Ints 4 => Species => Role => Str ; n1,n : NNumber ; g1 : AGender; nonEmpty : Bool} ;

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
      { s  = \\role => 
                 let spec = case cnn.nonEmpty of {True=>Indef; _=>quant.spec} ;
                     s    = quant.s ! True ! aform (gennum cnn.g1 (numnnum cnn.n)) (case role of {RVoc=>Indef; _=>Def}) role ++ 
                            conj.s ++ (linCoordSep [])!conj.distr!conj.conj ++ 
                            cnn.s ! conj.distr ! conj.conj ! spec ! role
                 in case role of {
                      RObj c => linCase c quant.p ++ s;
                      _      => s
                    } ;
        gn = gennum cnn.g1 (numnnum cnn.n);
        p  = NounP3 quant.p
      } ;
      
    ReflPossCNN conj cnn = {
        s = \\role => reflPron ! aform (gennum cnn.g1 (numnnum cnn.n1)) Def (RObj Acc) ++
                      conj.s ++ (linCoordSep [])!conj.distr!conj.conj ++ 
                      cnn.s ! conj.distr ! conj.conj ! Def ! role ;
        gn = gennum cnn.g1 (numnnum cnn.n)
      } ;
      
    PossCNN_RNP quant conj cnn rnp =
      { s = \\role =>
                let spec = case cnn.nonEmpty of {True=>Indef; _=>quant.spec} ;
                    s    = quant.s ! True ! aform (gennum cnn.g1 (numnnum cnn.n)) (case role of {RVoc=>Indef; _=>Def}) role ++ 
                           conj.s ++ (linCoordSep [])!conj.distr!conj.conj ++ 
                           cnn.s ! conj.distr ! conj.conj ! spec ! role ++
                           "на" ++ rnp.s ! (RObj CPrep)
                in case role of {
                     RObj c => linCase c quant.p ++ s;
                     _      => s
                   } ;
        gn = gennum cnn.g1 (numnnum cnn.n)
      } ;

lin NumMore num = {s = \\cf => "още" ++ num.s ! cf ;      nn = NNum Pl ; nonEmpty = True} ;
    NumLess num = {s = \\cf => num.s ! cf ++ "по-малко" ; nn = NNum Pl ; nonEmpty = True} ;

lin UseACard card = 
      {s  = table { CFMasc spec _ => card.s ! spec;
                    CFMascDefNom _ => card.s ! Def;
                    CFFem spec => card.s ! spec;
                    CFNeut spec => card.s ! spec 
                  };
       nn = card.nn
      };

    UseAdAACard ada card = 
      {s  = table { CFMasc spec _ => ada.s ++ card.s ! spec;
                    CFMascDefNom _ => ada.s ++ card.s ! Def;
                    CFFem spec => ada.s ++ card.s ! spec;
                    CFNeut spec => ada.s ++ card.s ! spec 
                  };
       nn = card.nn
      };

lin RelNP    = GrammarBul.RelNP ;
    ExtRelNP np rs = {
      s  = \\role => case role of {
                       RObj c => linCase c (personPol np.p) ++ np.s ! RObj CPrep ;
                       role   => np.s ! role
                     } ++ bindComma ++ rs.s ! personAgr np.gn np.p ;
      gn = np.gn ;
      p  = NounP3 (personPol np.p)
      } ;

lin BareN2 n2 = n2 ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ case pol.p of {Pos => []; Neg => "не"} ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 (GSg Neut)
    } ;

    CAdvAP pol cadv ap comp = {
      s = \\a,p => pol.s ++ case pol.p of {Pos => []; Neg => "не"} ++ cadv.s ++ ap.s ! a ! p ++ cadv.p ++ comp.s ! agrP3 (GSg Neut) ; 
      isPre = False
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ case pol.p of {Pos => []; Neg => "не"} ++ cadv.s ++ cadv.p} ;

    EnoughAP a ant pol vp = {
      s = \\aform,p => let gn = case aform of {
                                  ASg g _       => GSg g ;
                                  ASgMascDefNom => GSg Masc ;
                                  APl   _       => GPl
                                }
                       in "достатъчно" ++ a.s ! aform ! p ++ ant.s ++ pol.s ++ daComplex ant.a (orPol pol.p vp.p) vp ! Perf ! {gn=gn; p=p} ;
      isPre = False
    } ;

    EnoughAdv adv = {
      s = "достатъчно" ++ adv.s
    } ;

    ExtAdvAP ap adv = {
      s = \\aform,p => ap.s ! aform ! p ++ bindComma ++ adv.s ;
      isPre = False
    } ;

lin TimeNP np = {s = np.s ! RObj CPrep} ;

lin AdvAdv adv1 adv2 = {s=adv1.s ++ adv2.s} ;

lin UseDAP dap = {
      s  = \\role => let s = dap.s ! False ! ANeut ! role
                     in case role of {
                          RObj c => linCase c dap.p ++ s;
                          _      => s
                        } ;
      gn = gennum ANeut (numnnum dap.nn);
      p  = NounP3 dap.p
      } ;

    UseDAPMasc dap = {
      s  = \\role => let s = dap.s ! False ! (AMasc Human) ! role
                     in case role of {
                          RObj c => linCase c dap.p ++ s;
                          _      => s
                        } ;
      gn = gennum (AMasc Human) (numnnum dap.nn);
      p  = NounP3 dap.p
      } ;

    UseDAPFem dap = {
      s = \\role => let s = dap.s ! False ! AFem ! role
                    in case role of {
                         RObj c => linCase c dap.p ++ s;
                         _      => s
                       } ;
      gn = gennum AFem (numnnum dap.nn);
      p  = NounP3 dap.p
      } ;

lin AdvImp adv imp = {
      s = \\pol,gennum => adv.s ++ imp.s ! pol ! gennum
    } ;

lin whatSgFem_IP  = mkIP "каква" "каква" (GSg Fem) ;
    whatSgNeut_IP = mkIP "какво" "какво" (GSg Neut) ;

lin that_RP = IdRP ;

lin EmbedVP ant pol vp = {s = \\agr => ant.s ++ pol.s ++ daComplex ant.a (orPol pol.p vp.p) vp ! Perf ! agr} ;

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
      clitics = [] ;
      compl1 = \\agr => ant.s ++ pol.s ++ daComplex ant.a pol.p {s=slash.s; ad=slash.ad; clitics=slash.clitics; compl=slash.compl1; vtype=slash.vtype; p = Pos; isSimple = slash.isSimple} ! Perf ! agr ;
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
      clitics = [] ;
      compl1 = \\agr => ant.s ++ pol.s ++ vv.c2.s ++ np.s ! RObj vv.c2.c ++ 
                        daComplex ant.a (orPol pol.p (personPol np.p)) {s=slash.s; ad=slash.ad; clitics=slash.clitics; compl=slash.compl1; vtype=slash.vtype; p=Pos; isSimple = slash.isSimple} ! Perf ! personAgr np.gn np.p ;
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
      s = \\aform,_ => a.s ! aform ++ a.c2.s ++ rnp.s ! RObj a.c2.c ;
      isPre = False
      } ;

    ReflVPSlash slash rnp = {
      s   = slash.s ;
      ad  = slash.ad ;
      clitics = slash.clitics ;
      compl = \\a => slash.compl1 ! a ++ slash.c2.s ++ rnp.s ! RObj slash.c2.c ++ slash.compl2 ! agrP3 rnp.gn ;
      vtype = slash.vtype ;
      p     = slash.p ;
      isSimple = False
    } ;

lin RecipVPSlash slash = {
      s   = slash.s ;
      ad  = slash.ad ;
      clitics = slash.clitics ;
      compl = \\a => slash.compl1 ! a ++ "един"++linPrep slash.c2++"друг" ++ slash.compl2 ! a ;
      vtype = case slash.c2.c of {
                Acc | Dat => VMedial slash.c2.c ;
                _         => slash.vtype
              } ;
      p     = slash.p ;
      isSimple = False
      } ;

    RecipVPSlashCN slash cn = {
      s   = slash.s ;
      ad  = slash.ad ;
      clitics = slash.clitics ;
      compl = \\a => slash.compl1 ! a ++ linPrep slash.c2 ++ "един на друг"  ++ cn.s ! NF Pl Def ++ slash.compl2 ! a ;
      vtype = slash.vtype ;
      p     = slash.p ;
      isSimple = False
      } ;

lincat Sub1000000000 = {s : CardOrd => NumF => Str; n : Number} ;

lin pot3as4 n = n ;
    pot4 n = {
      s = \\o,f => case o of {
                     NCard cf   => let cf : CardForm =
                                         case cf of {
                                           CFMasc spec _  => CFMasc spec NonHuman ;
                                           CFMascDefNom _ => CFMascDefNom NonHuman ;
                                           CFFem  spec    => CFMasc spec NonHuman ;
                                           CFNeut spec    => CFMasc spec NonHuman
                                         }
                                   in n.s ! NCard cf ! f ++ case n.n of {Sg => "милион"; Pl => "милиона"} ;
                     NOrd aform => let cf : CardForm =
                                         case aform of {
                                           ASg Masc spec => CFMasc spec NonHuman ;
                                           ASgMascDefNom => CFMascDefNom NonHuman ;
                                           ASg Fem  spec => CFFem  spec ;
                                           ASg Neut spec => CFNeut spec ;
                                           APl spec      => CFNeut spec
                                         }
                                   in n.s ! NCard cf ! f ++ (mkA079 "милионен").s ! aform
                   } ;
      n = Pl
    } ;
    pot4plus n1 n2 = {
      s = \\o,f => (pot4 n1).s ! o ! f ++ "и" ++ n2.s ! o ! f;
      n = Pl
    } ;

    pot21 = {
      s = \\o,_ => mkCardOrd100 "сто" "стоте" "стотен" ! o ;
      i = False ;
      n = Pl
    } ;
    pot31 = {
      s = \\o,_ => mkCardOrd100 "хиляда" "хилядата" "хиляден" ! o ;
      n = Pl
    } ;
    pot41 = {
      s = \\o,_ => case o of {
                     NCard (CFMasc Indef _) => "милион" ;
                     NCard (CFMasc Def _)   => "единия милион" ;
                     NCard (CFMascDefNom _) => "единият милион" ;
                     NCard (CFFem  Indef)   => "милион" ;
                     NCard (CFFem  Def)     => "единия милион" ;
                     NCard (CFNeut Indef)   => "милион" ;
                     NCard (CFNeut Def)     => "единия милион" ;
                     NOrd  aform            => (mkA079 "милионен").s ! aform
                   };
      n = Pl
    } ;

    num x = {s = \\c => x.s ! c ! Formal; n=x.n} ;

}
	
