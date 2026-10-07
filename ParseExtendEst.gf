concrete ParseExtendEst of ParseExtend =
  ExtendEst - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralEst - [num], PunctuationX ** 
  open Prelude, ResEst, (P=ParadigmsEst), (G=GrammarEst), Coordination in {

lincat CNN = {
    s1,s2 : NPForm => Str ;
    r1,r2 : Agr => NPForm => Str ;
    n : Number
    } ;

oper
  reflPossNP : Agr -> Num -> CN -> NPhrase = \agr,num,cn ->
    G.DetCN (G.DetQuant (lin Quant {
      s,sp = \\_,_ => "oma" ;
      isDef = True
      }) num) cn ;

  recipNP : NPhrase =
    G.MassNP (G.UseN (P.mkN "teineteine" "teineteise" "teineteist"
                            "teineteisesse" "teineteiste" "teineteisi")) ;

lin UttAP  p ap  = {s = ap.s ! False ! NCase (complNumAgr p.a) Nom} ;

    UttVPS p vps = {s = vps.s ! p.a} ;

    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

lin RelNP = G.RelNP ;
    ExtRelNP = G.RelNP ;

lin BareN2 n = n ;

lin that_RP = G.IdRP ;

lin gen_Quant = G.DefArt ;

    ReflVPSlash vps rnp =
      insertObjPre (\\fin,b,agr => appCompl fin b vps.c2 (rnp2np agr rnp)) vps ;

    ReflA2 a rnp =
      let np = rnp2np (agrP3 Sg) rnp in {
        s = \\isMod,af => preOrPost isMod
              (appCompl True Pos a.c2 np) (a.s ! Posit ! AN af) ;
        infl = a.infl
        } ;

    BaseCNN num1 cn1 num2 cn2 =
      let np1 = G.DetCN (G.DetQuant G.DefArt num1) cn1 ;
          np2 = G.DetCN (G.DetQuant G.DefArt num2) cn2
      in {
        s1 = np1.s ; s2 = np2.s ;
        r1 = \\agr,npf => (reflPossNP agr num1 cn1).s ! npf ;
        r2 = \\agr,npf => (reflPossNP agr num2 cn2).s ! npf ;
        n = conjNumber num1.n num2.n
        } ;

    DetCNN quant conj cnn = emptyNP ** {
      s = \\npf => cnn.s1 ! npf ++ conj.s2 ++ cnn.s2 ! npf ;
      a = agrP3 (conjNumber cnn.n conj.n) ;
      isPron = False
      } ;

    ReflPossCNN conj cnn = {
      s = \\agr,npf => cnn.r1 ! agr ! npf ++ conj.s2 ++ cnn.r2 ! agr ! npf
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,npf => cnn.r1 ! agr ! npf ++ conj.s2 ++ cnn.r2 ! agr ! npf ++
                        rnp.s ! agr ! NPCase Gen
      } ;

    NumLess n = {s = \\number,c => n.s ! number ! c ++ "vähem" ; n = Pl ; isNum = True} ;
    NumMore n = {s = \\number,c => n.s ! number ! c ++ "rohkem" ; n = Pl ; isNum = True} ;

    UseACard card = {s = \\_,_ => card.s ; n = Pl} ;
    UseAdAACard ada card = {s = \\_,_ => ada.s ++ card.s ; n = Pl} ;

    ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Sg
      } ;

    CAdvAP pol cadv ap comp = ap ** {
      s = \\isMod,nf => pol.s ++ cadv.s ++ ap.s ! isMod ! nf ++ cadv.p ++
                         comp.s ! agrP3 Sg
      } ;

    AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.p} ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\isMod,nf => ap.s ! isMod ! nf ++ "piisavalt" ++
                         infVPAnt ant.a (NPCase Nom) pol.p (agrP3 Sg) vp InfDa
      } ;

    EnoughAdv adv = {s = adv.s ++ "piisavalt"} ;

    whatSgFem_IP, whatSgNeut_IP = G.whatSg_IP ;

    ExtAdvAP ap adv = ap ** {
      s = \\isMod,nf => ap.s ! isMod ! nf ++ SOFT_BIND ++ "," ++ adv.s
      } ;

    TimeNP np = {s = linNP NPAcc np} ;
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

    EmbedVP ant pol p vp = {
      s = infVPAnt ant.a (NPCase Nom) pol.p p.a vp InfDa
      } ;

    ComplVV vv ant pol vp =
      insertObj
        (\\_,_,agr => infVPAnt ant.a vv.sc pol.p agr vp vv.vi)
        (predV vv) ;

    SlashVV vv ant pol vps =
      insertObj
        (\\_,_,agr => infVPAnt ant.a vv.sc pol.p agr vps vv.vi)
        (predV vv) ** {c2 = vps.c2} ;

    SlashV2V v ant pol vp =
      insertObj
        (\\_,_,agr => infVPAnt ant.a v.sc pol.p agr vp v.vi)
        (predV v) ** {c2 = v.c2} ;

    SlashV2VNP v np ant pol vps =
      insertObjPre
        (\\fin,b,agr => appCompl fin b v.c2 np ++
                         infVPAnt ant.a v.sc pol.p agr vps v.vi)
        (predV v) ** {c2 = vps.c2} ;

    InOrderToVP ant pol p vp = {
      s = "et" ++ infVPAnt ant.a (NPCase Nom) pol.p p.a vp InfDa
      } ;

    CompVP ant pol p vp = {
      s = \\_ => infVPAnt ant.a (NPCase Nom) pol.p p.a vp InfDa
      } ;

    UttVP ant pol p vp = {
      s = infVPAnt ant.a (NPCase Nom) pol.p p.a vp InfDa
      } ;

    RecipVPSlash slash = G.ComplSlash slash recipNP ;
    RecipVPSlashCN slash cn =
      G.ComplSlash slash (reflPossNP (agrP3 Pl) G.NumSg cn) ;

    FocusComp comp np =
      mkClause (\_ -> comp.s ! np.a) np.a
        (insertObj (\\_,_,_ => linNP (NPCase Nom) np)
                   (predV (verbOlema ** {sc = NPCase Nom}))) ;

}
