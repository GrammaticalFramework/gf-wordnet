concrete ParseExtendEng of ParseExtend = 
  ExtendEng - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop], NumeralEng - [num], PunctuationX **
  open Prelude, ResEng, MorphoEng, GrammarEng, (E = ExtraEng), Coordination in {

lin gen_Quant = {
      s  = \\hasCard,n => "" ;
      sp = \\g,hasCard,n => case <n,hasCard> of {
        <Sg,False> => table { NCase Gen => table Gender ["its"; "his"; "her"] ! g; _ => table Gender ["it"; "he"; "she"] ! g } ;
        <Pl,False> => table { NCase Nom => "they"; NPAcc => "them"; _ => "theirs" } ;
        _          => \\c => artDef
        } ;
      isDef = True
      } ;

    UttAP     ap = {s = ap.s ! agrgP3 Sg Neutr} ;
    UttAPMasc ap = {s = ap.s ! agrgP3 Sg Masc} ;
    UttAPFem  ap = {s = ap.s ! agrgP3 Sg Fem } ;

    UttVPS     p vps = {s = vps.s ! p.a}  ;

    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    AdvRNP np prep rnp = {s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! a) vp ;
    AdvRAP ap prep rnp = {s = \\a => ap.s ! a ++ prep.s ++ rnp.s ! a ; isPre = False} ;

    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = \\_ => rnp.s ! pron.a; a = pron.a})) ;

lin FocusComp comp np = mkClause (comp.s ! np.a) np.a (insertObj (\\_ => np.s ! npNom) (predAux auxBe)) ;

lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp x y = twoTable Agr x y ;
    ConsComp xs x = consrTable Agr comma xs x ;
    ConjComp conj ss = conjunctDistrTable Agr conj ss ;

lincat CNN = {s1 : Bool => Case => Str; s2 : Case => Str; n1,n : Number; g1 : Gender; hasCard : Bool} ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\d,c => num1.s ! d     ! Nom ++ cn1.s ! num1.n ! c ;
      s2 = \\  c => num2.s ! False ! Nom ++ cn2.s ! num2.n ! c ;
      n1 = num1.n ;
      g1 = cn1.g ;
      n  = conjNumber num1.n num2.n ;
      hasCard = num1.hasCard
    } ;

    DetCNN quant conj cnn = {
      s = \\c => quant.s ! cnn.hasCard ! cnn.n1 ++ conj.s1 ++ cnn.s1 ! quant.isDef ! npcase2case c ++ conj.s2 ++ cnn.s2 ! npcase2case c ;
      a = conjAgr (agrP3 conj.n) (agrgP3 cnn.n cnn.g1)
    } ;

    ReflPossCNN conj cnn = {s = \\a => possPron ! a ++ conj.s1 ++ cnn.s1 ! True ! Nom ++ conj.s2 ++ cnn.s2 ! Nom} ;
    
    PossCNN_RNP quant conj cnn rnp = {
      s = \\a => quant.s ! cnn.hasCard ! cnn.n1 ++ conj.s1 ++ cnn.s1 ! quant.isDef ! Nom ++ conj.s2 ++ cnn.s2 ! Nom ++ "of" ++ rnp.s ! a ;
    } ;

lin NumLess num = {
      s,sp = \\d,c => num.sp ! d ! c ++ "less";
      n = num.n;
      hasCard = True
    } ;

    NumMore num = {
      s,sp = \\d,c => num.sp ! d ! c ++ "more";
      n = num.n;
      hasCard = True
    } ;

    UseACard acard = {s,sp = \\_ => acard.s; n = acard.n} ;
    UseAdAACard ada acard = {s,sp = \\_,c => ada.s ++ acard.s ! c; n = acard.n} ;

lin RelNP np rs = {
      s = \\c => np.s ! c ++ rs.s ! np.a ++ finalComma ;
      a = np.a
      } ;
    ExtRelNP = GrammarEng.RelNP ;

lin BareN2 n2 = n2 ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ! (case pol.p of {CPos => Pos; CNeg _ => Neg}) ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Sg
    } ;

    CAdvAP pol cadv ap comp = {
      s = \\a => pol.s ++ cadv.s ! (case pol.p of {CPos => Pos; CNeg _ => Neg}) ++ ap.s ! a ++ cadv.p ++ comp.s ! a ;
      isPre = False
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ! (case pol.p of {CPos => Pos; CNeg _ => Neg}) ++ cadv.p} ;

    EnoughAP a ant pol vp = {
      s = \\agr => a.s ! agr ++ "enough" ++ ant.s ++ pol.s ++ infVP VVInf vp (variants {True; False}) ant.a pol.p agr ;
      isPre = False
    } ;
    
    EnoughAdv adv = {
      s = adv.s ++ "enough"
    } ;

    ExtAdvAP ap adv = {s = \\a => ap.s ! a ++ bindComma ++ adv.s ; isPre = False} ;

lin TimeNP np = {s = np.s ! NPAcc} ;

lin AdvAdv adv1 adv2 = {s=adv1.s ++ adv2.s} ;

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

lin EmbedVP ant pol p vp = {s = 
      ant.s ++ pol.s ++ 
      variants {infVP VVInf vp True  ant.a pol.p p.a ;
                infVP VVInf vp False ant.a pol.p p.a}
      } ;

    ComplVV v ant pol vp = 
      insertObj (variants {\\agr => ant.s ++ pol.s ++ 
                                    infVP v.typ vp True  ant.a pol.p agr;
                           \\agr => ant.s ++ pol.s ++ 
                                    infVP v.typ vp False ant.a pol.p agr})
                (predVV v) ;

    SlashVV vv ant pol vp = vp **
      insertObj (variants {\\a => ant.s ++ pol.s ++ infVP vv.typ vp True  ant.a pol.p a ;
                           \\a => ant.s ++ pol.s ++ infVP vv.typ vp False ant.a pol.p a})
                (predVV vv) ;

    SlashV2V v ant pol vp =
      insertObjc (variants {\\a => ant.s ++ pol.s ++ v.c3 ++ 
                                   infVP v.typ vp True  ant.a pol.p a ;
                            \\a => ant.s ++ pol.s ++ v.c3 ++ 
                                   infVP v.typ vp False ant.a pol.p a})
                 (predVc v) ;

    SlashV2VNP vv np ant pol vp = vp **
      insertObjPre (\\_ => vv.c2 ++ np.s ! NPAcc)
                   (insertObjc (variants {\\a => ant.s ++ pol.s ++ vv.c3 ++ infVP vv.typ vp True  ant.a pol.p a ;
                                          \\a => ant.s ++ pol.s ++ vv.c3 ++ infVP vv.typ vp False ant.a pol.p a})
                               (predVc vv)) ;

    InOrderToVP ant pol p vp = {s = 
      variants {"in order"; []} ++ ant.s ++ pol.s ++
      infVP VVInf vp (variants {True; False}) ant.a pol.p p.a
      } ;

    CompVP ant pol p vp = {s = variants {\\a => ant.s ++ pol.s ++ 
                                                infVP VVInf vp True  ant.a pol.p p.a ;
                                         \\a => ant.s ++ pol.s ++ 
                                                infVP VVInf vp False ant.a pol.p p.a}} ;

    UttVP ant pol p vp = {s = ant.s ++ pol.s ++ infVP VVInf vp (variants {True; False}) ant.a pol.p p.a} ;

    ReflVPSlash vps rnp = insertObj (\\a => vps.c2 ++ rnp.s ! a) vps ;

    ReflA2 a rnp = {
      s = \\ag => a.s ! AAdj Posit Nom ++ a.c2 ++ rnp.s ! ag ; 
      isPre = False
      } ;

lin RecipVPSlash slash = GrammarEng.ComplSlash slash (variants {mkNP "each other"  "each other"  "each other"  Sg P3 Masc ;
                                                                mkNP "one another" "one another" "one another" Sg P3 Masc});
    RecipVPSlashCN slash cn = GrammarEng.ComplSlash slash (DetCN (variants {mkDeterminer Sg "each other's" ;
                                                                            mkDeterminer Sg "one another's"}) cn);

lincat Sub1000000000 = {s : Bool => CardOrd => Case => Str ; n : Number} ;

lin pot3as4 n = n ;
    pot4 n = {
      s = \\d,o,c => n.s ! d ! NCard ! Nom ++ pot41.s ! True ! o ! c ;
      n = Pl
    } ;
    pot4plus n1 n2 = {
      s = \\d,o,c => n1.s ! d ! NCard ! Nom ++ pot41.s ! True ! NCard ! Nom ++ "and" ++ n2.s ! True ! o ! c;
      n = Pl
    } ;

    pot21 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "hundred").s ! o ! c;
      n = Pl
    } ;
    pot31 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "thousand").s ! o ! c;
      n = Pl
    } ;
    pot41 = {
      s = \\d,o,c => case d of {True => []; False => "a"} ++
                     (regCardOrd "million").s ! o ! c;
      n = Pl
    } ;

    num x = x ;

lincat ListImp = {s1,s2 : CPolarity => ImpForm => Str} ;
lin BaseImp = twoTable2 CPolarity ImpForm ;
    ConsImp = consrTable2 CPolarity ImpForm comma ;
    ConjImp conj ss = conjunctDistrTable2 CPolarity ImpForm conj ss ;

lin ProgrVPSlash vp = 
      insertObj (\\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a) (predAux auxBe) **
        { c2 = vp.c2 ;
          gapInMiddle = vp.gapInMiddle ;
          missingAdv = vp.missingAdv
        } ;

}
