concrete ParseExtendAra of ParseExtend =
 CatAra,
 ExtendAra - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
 NumeralAra - [num],
 PunctuationX
  ** open Prelude, ResAra, GrammarAra, ParadigmsAra, (S=StructuralAra), (P=ParamX) in {

lin gen_Quant = baseQuant ** {
      s = \\_,_,_,_ => [] ;
      d = Def ;
      isEmpty = True
    } ;

    UttAP p ap = {s = \\_ => ap.s ! NoHum ! (pgn2gn p.a.pgn).g
                                      ! (pgn2gn p.a.pgn).n ! Indef ! Nom} ;
    UttVPS p vps = {s = \\_ => vps.s ! p.a.pgn} ;

lin
    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ! masc ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = GrammarAra.RelNP ;
    ExtRelNP = GrammarAra.RelNP ;

lin BareN2 n = n ;

lin ReflA2 a rnp = GrammarAra.ComplA2 a rnp ;
    ReflVPSlash slash rnp = GrammarAra.ComplSlash slash rnp ;

lincat CNN = {n1,n2 : Size ; cn1,cn2 : CN} ;

lin BaseCNN n1 cn1 n2 cn2 = {n1 = n1.n ; cn1 = cn1 ; n2 = n2.n ; cn2 = cn2} ;

    DetCNN quant conj cnn =
      GrammarAra.ConjNP conj
        (GrammarAra.BaseNP
          (GrammarAra.DetCN (GrammarAra.DetQuant quant (lin Num {s = \\_,_,_ => [] ; n = cnn.n1 ; isNum = False})) cnn.cn1)
          (GrammarAra.DetCN (GrammarAra.DetQuant quant (lin Num {s = \\_,_,_ => [] ; n = cnn.n2 ; isNum = False})) cnn.cn2)) ;

    ReflPossCNN conj cnn = lin RNP
      (DetCNN (GrammarAra.PossPron S.he_Pron) conj cnn) ;

    PossCNN_RNP quant conj cnn rnp = lin RNP
      (GrammarAra.MassNP
        (GrammarAra.PossNP
          (GrammarAra.PossNP (GrammarAra.UseN (mkN "مجموع")) (DetCNN quant conj cnn))
          rnp)) ;

lin NumLess n = n ** {s = \\g,d,c => n.s ! g ! d ! c ++ "أقل"} ;
    NumMore n = n ** {s = \\g,d,c => n.s ! g ! d ! c ++ "إضافي"} ;
    UseACard c = {s = \\_,_,_ => c.s ; n = None ; isNum = False} ;
    UseAdAACard ada c = {s = \\_,_,_ => ada.s ++ c.s ; n = None ; isNum = False} ;

lin ExtAdvAP ap adv = ap ** {
      s = \\h,g,n,d,c => ap.s ! h ! g ! n ! d ! c ++ adv.s
    } ;

    ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p
        ++ comp.s ! {g = Masc ; n = Sg} ! Acc ++ comp.obj.s
      } ;

    CAdvAP pol cadv ap comp = ap ** {
      s = \\h,g,n,d,c => pol.s ++ cadv.s ++ ap.s ! h ! g ! n ! d ! c
                       ++ cadv.p ++ comp.s ! {g = g ; n = n} ! Acc
                       ++ comp.obj.s
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ cadv.s} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\h,g,n,d,c => ap.s ! h ! g ! n ! d ! c ++ "بِمَا يَكْفِي لِـ"
                       ++ finiteVP ant pol (Per3 g n) vp
      } ;

    EnoughAdv adv = {s = adv.s ++ "بِمَا يَكْفِي"} ;

    TimeNP np = {s = np.s ! Acc} ;
    AdvAdv a b = {s = a.s ++ b.s} ;

    whatSgFem_IP = S.whatSg_IP ** {a = {pgn = Per3 Fem Sg ; isPron = False}} ;
    whatSgNeut_IP = S.whatSg_IP ;
    that_RP = IdRP ;

lin EmbedVP ant pol pron vp = {s = finiteVP ant pol pron.a.pgn vp} ;

    ComplVV vv ant pol vp =
      let main = predV vv in main ** {
        s = \\pgn,vpf => main.s ! pgn ! vpf
                       ++ vv.s2 ++ finiteVP ant pol pgn vp ;
        obj = emptyObj ;
        s2 = []
      } ;

    SlashVV vv ant pol slash = slash ** {
      s = \\pgn,vpf => (predV vv).s ! pgn ! vpf
                     ++ vv.s2 ++ finiteVP ant pol pgn (lin VP slash) ;
      s2 = [] ;
      obj = emptyObj ;
      agrObj = slash.agrObj ;
      vtype = NotPred ;
      sc = vv.sc
    } ;

    SlashV2V v ant pol vp =
      let main = slashV2 v in main ** {
        agrObj = \\pgn => v.s2 ++ finiteVP ant pol pgn vp
      } ;

    SlashV2VNP v np ant pol slash =
      let main = slashV2 v in main ** {
        s2 = bindIfPron np main ++ v.s2 ++ finiteVP ant pol np.a.pgn (lin VP slash) ;
        c2 = slash.c2 ;
        agrObj = slash.agrObj
      } ;

    InOrderToVP ant pol pron vp = {s = "لِكَيْ" ++ finiteVP ant pol pron.a.pgn vp} ;
    CompVP ant pol pron vp = {
      s = \\_,_ => finiteVP ant pol pron.a.pgn vp ;
      obj = emptyObj ;
      isNP = False
    } ;
    UttVP ant pol pron vp = {s = \\_ => finiteVP ant pol pron.a.pgn vp} ;

lin RecipVPSlash slash = GrammarAra.ComplSlash slash reciprocalNP ;
    RecipVPSlashCN slash cn = GrammarAra.ComplSlash slash
      (GrammarAra.MassNP (GrammarAra.PossNP cn reciprocalNP)) ;

    FocusComp comp np = GrammarAra.PredVP np (GrammarAra.UseComp comp) ;

oper reciprocalNP : NP = lin NP (indeclNP "بَعْضُهُمْ بَعْضًا" Pl) ;

     finiteVP : Ant -> Pol -> PerGenNum -> VP -> Str = \ant,pol,pgn,vp ->
       let tense = case ant.a of {P.Simul => P.Pres ; P.Anter => P.Past}
       in vStr vp pgn tense pol.p Subord
          ++ vp.obj.s
          ++ pred vp pgn tense pol.p
          ++ vp.s2 ;

}
