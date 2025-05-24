concrete ParseExtendBul of ParseExtend =
  ExtendBul - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralBul - [num], PunctuationX ** open Predef, Prelude, ResBul, GrammarBul, ParadigmsBul in {

lin gen_Quant = DefArt ;

    UttAP  p ap  = {s = ap.s ! aform p.gn Indef RSubj ! P3} ;
    UttVPS p vps = {s = vps.s ! personAgr p.gn p.p} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin FocusComp comp np =
      mkClause (comp.s ! personAgr np.gn np.p) np.gn (NounP3 comp.p)
               (insertObj (\\_ => np.s ! RSubj) (personPol np.p) (predV verbBe)) ;

lincat CNN = {s : Species => Role => Ints 4 => Str ; n1,n : NNumber ; g1 : AGender; nonEmpty : Bool} ;

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
      in { s  = \\spec,role => table {4 => num2.s ! dgenderSpecies cn2.g spec role ++ cn2.s ! mknf num2.nn spec role cn2.g ;
                                      _ => num1.s ! dgenderSpecies cn1.g spec role ++ cn1.s ! mknf num1.nn spec role cn1.g
                                     } ;
           n1 = num1.nn ;
           n  = num1.nn ;
           g1 = cn1.g ;
           nonEmpty = num1.nonEmpty
      } ;

    DetCNN quant conj cnn =
      { s  = \\role =>
                 let spec = case cnn.nonEmpty of {True=>Indef; _=>quant.spec} ;
                     s    = quant.s ! True ! aform (gennum cnn.g1 (numnnum cnn.n)) (case role of {RVoc=>Indef; _=>Def}) role ++
                            linCoord [] ! conj.sep ++ cnn.s ! spec ! role ! conj.sep ++ conj.s ++ cnn.s ! spec ! role ! 4
                 in case role of {
                      RObj c => linCase c quant.p ++ s;
                      _      => s
                    } ;
        gn = gennum cnn.g1 (numnnum cnn.n);
        p  = NounP3 quant.p
      } ;

    ReflPossCNN conj cnn = {
        s = \\role => reflPron ! aform (gennum cnn.g1 (numnnum cnn.n1)) Def (RObj Acc) ++
                      cnn.s ! Def ! role ! conj.sep ++ conj.s ++ cnn.s ! Def ! role ! 4 ;
        gn = gennum cnn.g1 (numnnum cnn.n) ;
        isPron = False
      } ;

    PossCNN_RNP quant conj cnn rnp =
      { s = \\role =>
                let spec = case cnn.nonEmpty of {True=>Indef; _=>quant.spec} ;
                    s    = quant.s ! True ! aform (gennum cnn.g1 (numnnum cnn.n)) (case role of {RVoc=>Indef; _=>Def}) role ++
                           cnn.s ! spec ! role ! conj.sep ++ conj.s ++ cnn.s ! spec ! role ! 4 ++
                           "на" ++ rnp.s ! (RObj CPrep)
                in case role of {
                     RObj c => linCase c quant.p ++ s;
                     _      => s
                   } ;
        gn = gennum cnn.g1 (numnnum cnn.n) ;
        isPron = False
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

lin whatSgFem_IP  = mkIP "каква" "каква" (GSg Fem) ;
    whatSgNeut_IP = mkIP "какво" "какво" (GSg Neut) ;

lin that_RP = IdRP ;

lin EmbedVP ant pol p vp = {s = ant.s ++ pol.s ++ daComplex ant.a (orPol pol.p vp.p) vp ! Perf ! personAgr p.gn p.p} ;

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

    InOrderToVP ant pol p vp =
      {s = "за" ++ ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! personAgr p.gn p.p};

    CompVP ant pol p vp = {s = \\agr => ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! personAgr p.gn p.p; p = Pos} ;

    UttVP ant pol p vp = {s = ant.s ++ pol.s ++ daComplex ant.a pol.p vp ! Perf ! personAgr p.gn p.p} ;

    ReflA2 = ExtendBul.ReflA2RNP ;
    ReflVPSlash = ExtendBul.ReflRNP ;

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

lin num x = {s = \\c => x.s ! c ! Formal; n=x.n} ;

}
