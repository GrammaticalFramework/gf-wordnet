concrete ParseExtendDan of ParseExtend =
  ExtendDan - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, ProgrVPSlash, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralDan - [num], PunctuationX **
  open Prelude, ResDan, MorphoDan, CommonScand, GrammarDan, Coordination, ExtendDan, (M = MakeStructuralDan), (P = ParadigmsDan), (I = IrregDan) in {

lin gen_Quant = DefArt ;

    UttAP  p ap  = {s = ap.s ! Strong (gennum p.a.g p.a.n)} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin FocusComp comp np = mkClause (comp.s ! np.a) np.a (insertObj (\\_ => np.s ! MorphoDan.nominative) (predV verbBe)) ;

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

lin NumMore num = {s = \\g => num.s ! g ++ "mera" ;  isDet = num.isDet ; n = Pl} ;
    NumLess num = {s = \\g => num.s ! g ++ "färre" ; isDet = num.isDet ; n = Pl} ;

lin UseACard card =
      {s = \\_ => card.s;
       n = card.n
      };

    UseAdAACard ada card =
      {s = \\_ => ada.s ++ card.s;
       n = card.n
      };

lin RelNP np rs = {
      s = \\c => np.s ! c ++ rs.s ! np.a ! RNom ;
      a = np.a ;
      p = np.p ;
      isPron = False
      } ;
    ExtRelNP = GrammarDan.RelNP ;

lin BareN2 n2 = n2 ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ case pol.p of {Pos => []; Neg => "inte"} ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Neutr Sg
    } ;

    CAdvAP pol cadv ap comp = {
      s = \\a => let agr = case a of {
                             Strong (GSg g) => agrP3 g Sg;
                             Strong GPl     => agrP3 Neutr Pl;
                             Weak n         => agrP3 Neutr n
                           }
                  in pol.s ++ case pol.p of {Pos => []; Neg => "inte"} ++ cadv.s ++ ap.s ! a ++ cadv.p ++ comp.s ! agr ;
      isPre = False
    } ;

    AdnCAdv pol cadv = {s = pol.s ++ case pol.p of {Pos => []; Neg => "inte"} ++ cadv.s ++ conjThan} ;

    EnoughAP a ant pol vp = {
      s = \\ap => let agr = case ap of {
                              Strong (GSg g) => agrP3 g Sg;
                              Strong GPl     => agrP3 Neutr Pl;
                              Weak n         => agrP3 Neutr n
                            }
                  in a.s ! ap ++ "nog för" ++ infMark ++ ant.s ++ pol.s ++ infVPPlus vp agr ant.a pol.p ;
      isPre = False
    } ;

    EnoughAdv adv = {
      s = adv.s ++ "nog"
    } ;

    ExtAdvAP ap adv = {
      s = \\a => ap.s ! a ++ bindComma ++ adv.s ;
      isPre = ap.isPre
      } ;

lin TimeNP np = {s = np.s ! accusative} ;

lin AdvAdv adv1 adv2 = {s=adv1.s ++ adv2.s} ;

lin whatSgFem_IP, whatSgNeut_IP = whatSg_IP ;

lin that_RP = IdRP ;

lin EmbedVP ant pol p vp = {s = infMark ++ ant.s ++ pol.s ++ infVPPlus vp p.a ant.a pol.p} ;

    ComplVV vv ant pol vp = insertObjPost (\\a => vv.c2.s ++ ant.s ++ pol.s ++ infVPPlus vp a ant.a pol.p) (predV vv) ;

    SlashVV vv ant pol slash =
      insertObj (\\a => vv.c2.s ++ ant.s ++ pol.s ++ infVPPlus slash a ant.a pol.p) (predV vv) ** {n3 = slash.n3 ; c2 = slash.c2} ;

    SlashV2V v ant pol vp = predV v ** {
      n3 = \\a => v.c3.s ++ ant.s ++ pol.s ++ infVPPlus vp a ant.a pol.p ;
      c2 = v.c2
      } ;

    SlashV2VNP vv np ant pol vp =
      insertObj
        (\\a => vv.c2.s ++ np.s ! accusative ++ vv.c3.s ++ ant.s ++ pol.s ++ infVPPlus vp a ant.a pol.p) (predV vv)
        ** {n3 = vp.n3 ; c2 = vv.c2} ;

    InOrderToVP ant pol p vp = {  -- infinitive: att dricka öl, att vara glad
      s = "för att" ++ ant.s ++ pol.s ++ infVPPlus vp p.a ant.a pol.p
    } ;

    CompVP ant pol p vp = {s = \\agr => "att" ++ ant.s ++ pol.s ++ infVPPlus vp p.a ant.a pol.p} ;

    UttVP ant pol p vp = {s = infMark ++ ant.s ++ pol.s ++ infVPPlus vp p.a ant.a pol.p} ;

lin RecipVPSlash slash = GrammarDan.ComplSlash slash (regNP "varandra" "varandra" Utr Sg);
    RecipVPSlashCN slash cn = GrammarDan.ComplSlash slash (DetCN (M.mkDet "varandras" Pl) cn);

}
