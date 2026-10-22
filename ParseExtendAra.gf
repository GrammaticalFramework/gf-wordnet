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

lincat CNN = NumCN * NumCN ;

lin BaseCNN n1 cn1 n2 cn2 = <numCN n1 cn1,numCN n2 cn2> ;

oper
  NumCN = {num, noun, adj : State => Case => Str ;
           n : Size; h : Species; g : Gender
          } ;
  numCN : Num -> CN -> NumCN = \num,cn ->
    let number = case cn.isDual of {
                   True  => case sizeToNumber num.n of {
                              Sg => Sg ;
                              _  => Dl
                            } ;
                   False => sizeToNumber num.n
                 }
    in {num = \\d,c => num.s ! detGender cn.g num.n ! (toDef d num.n) ! c ;
        noun = \\d,c => cn.s ! number
                             ! nounState d number
                             ! nounCase c num.n d ;
        adj = \\d,c => cn.s2 ! number
                             ! (definite ! d) -- Indef remains Indef, rest become Def
                             ! c
                       ++ cn.np ! c ;
        n = num.n ;
        h = cn.h ;
        g = cn.g
    } ;

  quantCNN : Quant -> NumCN -> NP = \quant,cnn ->
    let cas : Case -> Case = if_then_else Case quant.is1sg Bare ;
        determiner : Case -> Str = \c ->
          quant.s ! sizeToNumber cnn.n ! cnn.h ! cnn.g ! c
          ++ cnn.num ! quant.d ! c ;
    in lin NP emptyNP ** {
      s = \\c => -- Dat is just a hack for liPrep
        let c' = case c of {Dat => Gen ; x => x} in
        case cnB4det (quant**{n=cnn.n}) of {
          False => determiner c'
                ++ cnn.noun ! quant.d ! c'
                ++ cnn.adj ! quant.d ! c' ;
          True => cnn.noun ! quant.d ! (cas c) -- deal with possessive suffix + dative hack
               ++ determiner c'
               ++ cnn.adj ! quant.d ! c'
        };
      a = { pgn = agrP3 cnn.h cnn.g (sizeToNumber cnn.n);
            isPron = False } ;
      isHeavy = True
    } ;


lin DetCNN quant conj cnn =
      GrammarAra.ConjNP conj
        (GrammarAra.BaseNP
          (quantCNN quant cnn.p1)
          (quantCNN quant cnn.p2)) ;

    ReflPossCNN conj cnn =
      (DetCNN (GrammarAra.PossPron S.he_Pron) conj cnn) ;

    PossCNN_RNP quant conj cnn rnp =
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
