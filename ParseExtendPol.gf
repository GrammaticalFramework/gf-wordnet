concrete ParseExtendPol of ParseExtend =
 ExtendPol - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash,
               ListComp, BaseComp, ConsComp, ConjComp,
               ListImp, BaseImp, ConsImp, ConjImp],
 NumeralPol - [num], PunctuationX ** open Prelude, ResPol, VerbMorphoPol, GrammarPol in {

lincat
    CNN = {np1,np2 : NounPhrase} ;
    [Comp] = {s1,s2 : GenNum => Str} ;
    [Imp] = {s1,s2 : Polarity => Number => Str} ;

lin
    UttAP  p ap = {s = ap.s ! AF p.gn Nom} ;
    UttVPS p vps = {s = vps.s ! p.gn ! p.p} ;

    gen_Quant = IndefArt ;

    ReflA2 a rnp = ComplA2 a (lin NP rnp) ;
    ReflVPSlash vps rnp = ComplSlash vps (lin NP rnp) ;

    BaseCNN n1 cn1 n2 cn2 = {
      np1 = DetCN (DetQuant IndefArt n1) cn1;
      np2 = DetCN (DetQuant IndefArt n2) cn2
      } ;
    DetCNN quant conj cnn = ConjNP conj (BaseNP cnn.np1 cnn.np2) ;
    ReflPossCNN conj cnn = lin RNP
      (ConjNP conj (BaseNP cnn.np1 cnn.np2)) ;
    PossCNN_RNP quant conj cnn rnp = lin RNP
      (ConjNP conj (BaseNP cnn.np1 cnn.np2)) ;

    BaseComp x y = {s1=x.s; s2=y.s} ;
    ConsComp x xs = {
      s1 = \\gn => x.s ! gn ++ "," ++ xs.s1 ! gn;
      s2 = xs.s2
      } ;
    ConjComp conj xs = {
      s = \\gn => conj.s1 ++ xs.s1 ! gn ++ conj.s2 ++ xs.s2 ! gn
      } ;

    BaseImp x y = {s1=x.s; s2=y.s} ;
    ConsImp x xs = {
      s1 = \\p,n => x.s ! p ! n ++ "," ++ xs.s1 ! p ! n;
      s2 = xs.s2
      } ;
    ConjImp conj xs = {
      s = \\p,n => conj.s1 ++ xs.s1 ! p ! n ++ conj.s2 ++ xs.s2 ! p ! n
      } ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin
    UttVP ant pol p vp = {
        s = vp.prefix ++
            pol.s ++
            infinitive_form vp.verb vp.imienne pol.p p.gn ++ 
            vp.sufix ! pol.p ! MascPersSg
    };

lin
    NumLess n = n ** {s = \\c,g => n.s ! c ! g ++ "mniej"} ;
    NumMore n = n ** {s = \\c,g => n.s ! c ! g ++ "więcej"} ;
    UseACard c = {s = \\_,_ => c.s; a=PiecA; n=Pl} ;
    UseAdAACard ada c = {s = \\_,_ => ada.s ++ c.s; a=PiecA; n=Pl} ;

    ExtAdvAP ap adv = ap ** {
      s = \\af => ap.s ! af ++ "," ++ adv.s;
      adv = ap.adv ++ "," ++ adv.s
      } ;
    ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! NeutSg
      } ;
    CAdvAP pol cadv ap comp = ap ** {
      s = \\af => case af of {
        AF gn _ => pol.s ++ cadv.s ++ ap.s ! af ++ cadv.p ++ comp.s ! gn
        };
      adv = pol.s ++ cadv.s ++ ap.adv ++ cadv.p ++ comp.s ! NeutSg
      } ;
    AdnCAdv pol cadv = {s = pol.s ++ cadv.s} ;
    EnoughAP ap ant pol vp = ap ** {
      s = \\af => case af of {
        AF gn _ => ap.s ! af ++ "wystarczająco, aby" ++
          vp.prefix ++ infinitive_form vp.verb vp.imienne pol.p gn ++
          vp.sufix ! pol.p ! gn
        };
      adv = ap.adv ++ "wystarczająco"
      } ;
    EnoughAdv adv = {s = adv.s ++ "wystarczająco"} ;
    TimeNP np = {s = np.dep ! AccNoPrep} ;
    AdvAdv a b = {s = a.s ++ b.s} ;

    whatSgFem_IP = {
      nom,voc = "co"; dep = \\_ => "co"; gn=FemSg; p=P3
      } ;
    whatSgNeut_IP = {
      nom,voc = "co"; dep = \\_ => "co"; gn=NeutSg; p=P3
      } ;

    EmbedVP ant pol pron vp = {
      s = vp.prefix ++ infinitive_form vp.verb vp.imienne pol.p pron.gn ++
          vp.sufix ! pol.p ! pron.gn
      } ;
    ComplVV vv ant pol vp = setSufix (defVP vv)
      (\\_,gn => vp.prefix ++ infinitive_form vp.verb vp.imienne pol.p gn ++
                 vp.sufix ! pol.p ! gn) ;
    SlashVV vv ant pol vps = setPostfix
      (setSufix (defVP vv)
        (\\_,gn => vps.prefix ++ infinitive_form vps.verb vps.imienne pol.p gn ++
                   vps.sufix ! pol.p ! gn))
      vps.postfix vps.c ;
    SlashV2V v ant pol vp = setPostfix (defVP (castv2 v))
      (\\_,gn => vp.prefix ++ infinitive_form vp.verb vp.imienne pol.p gn ++
                 vp.sufix ! pol.p ! gn) v.c ;
    SlashV2VNP v np ant pol vps = setPostfix
      (setSufix (defVP (castv2 v))
        (\\p,gn => v.c.s ++ np.dep ! (npcase ! <p,v.c.c>) ++
                   vps.prefix ++ infinitive_form vps.verb vps.imienne pol.p gn ++
                   vps.sufix ! pol.p ! gn))
      vps.postfix vps.c ;
    InOrderToVP ant pol pron vp = {
      s = "aby" ++ vp.prefix ++ infinitive_form vp.verb vp.imienne pol.p pron.gn ++
          vp.sufix ! pol.p ! pron.gn
      } ;
    CompVP ant pol pron vp = {
      s = \\_ => vp.prefix ++ infinitive_form vp.verb vp.imienne pol.p pron.gn ++
          vp.sufix ! pol.p ! pron.gn
      } ;

    RecipVPSlash vps = setSufix2 vps
      (\\p,gn => vps.sufix ! p ! gn ++ vps.c.s ++
                 "siebie nawzajem" ++ vps.postfix ! p ! gn) ;
    RecipVPSlashCN vps cn = setSufix2 vps
      (\\p,gn => vps.sufix ! p ! gn ++ vps.c.s ++
        cn.s ! Pl ! (extract_case ! vps.c.c) ++ "siebie nawzajem" ++
        vps.postfix ! p ! gn) ;
    FocusComp comp np = PredVP np (UseComp comp) ;

lin num a = { s = \\x,y=>a.s!<x,y>; o=a.o; a=a.a; n=a.n };

lin RelNP = GrammarPol.RelNP ;
    ExtRelNP = GrammarPol.RelNP ;

lin BareN2 n = n ;

lin that_RP = IdRP ;

}
