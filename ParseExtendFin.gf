concrete ParseExtendFin of ParseExtend =
  ExtendFin - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralFin - [num], PunctuationX **
 open MorphoFin, ResFin, ParadigmsFin, StemFin, (G=GrammarFin), (S=SyntaxFin), Coordination, Prelude in {

lincat
  CNN = {
    s1,s2 : NPForm => Str ;
    r1,r2 : Agr => NPForm => Str ;
    n : Number ;
    isNeg : Bool
    } ;

oper
  reflPossNP agr num cn =
    let
      quant : Quant = lin Quant {
        s1 = \\_,_ => [] ;
        sp = \\_,_ => [] ;
        s2 = \\harm => possSuffixGen harm agr ;
        isPoss = True ;
        isDef = True ;
        isNeg = False
        } ;
      det = G.DetQuant quant num
    in
      G.DetCN det cn ;

  recipNP = {
    s = \\_ => "toisiaan" ;
    a = agrP3 Pl ;
    isPron = True ;
    isNeg = False
    } ;

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    ComplVV v ant pol vp =
      insertObj
        (\\_,b,a => infVPGen pol.p v.sc b a vp (vvtype2infform v.vi))
        (predSV {s = v.s ;
                sc = case vp.s.sc of {
                  SCNom => v.sc ;        -- minun täytyy pestä auto
                  c     => c             -- minulla täytyy olla auto
                  } ;
                h = v.h ; p = v.p
               }
         ) ;

    UttAP  p ap  = {s = ap.s ! False ! NCase (complNumAgr p.a) Nom} ;
	UttVPS p vps = {s = vps.s ! p.a} ;

    SlashV2V v ant p vp =
      insertObj (\\_,b,a => infVPGen p.p v.sc b a vp (vvtype2infform v.vi)) (predSV v) ** {c2 = v.c2} ;

    SlashVV v ant pol vp = {
      s = v ;
      s2 = \\_,b,a => infVPGen pol.p v.sc b a vp (vvtype2infform v.vi) ;
      adv = \\_ => v.p ;
      vptyp = vp.vptyp ;
      ext = [] ;
      c2 = vp.c2
      } ;

    SlashV2VNP v np ant pol vp = slashV2VNP v np vp ;

    EmbedVP ant pol p vp = {
      s = infVPGen pol.p SCNom Pos p.a vp Inf1
      } ;

    InOrderToVP ant pol p vp = {
      s = infVPGen pol.p SCNom Pos p.a vp Inf1Long
      } ;

    CompVP ant pol p vp = {
      s = \\_ => infVPGen pol.p SCNom Pos p.a vp Inf1
      } ;

    UttVP ant pol p vp = {
      s = infVPGen pol.p SCNom Pos p.a vp Inf1
      } ;

    BaseCNN num1 cn1 num2 cn2 =
      let
        np1 = G.DetCN (G.DetQuant G.DefArt num1) cn1 ;
        np2 = G.DetCN (G.DetQuant G.DefArt num2) cn2
      in {
        s1 = np1.s ;
        s2 = np2.s ;
        r1 = \\agr,npf => (reflPossNP agr num1 cn1).s ! npf ;
        r2 = \\agr,npf => (reflPossNP agr num2 cn2).s ! npf ;
        n = conjNumber num1.n num2.n ;
        isNeg = orB np1.isNeg np2.isNeg
        } ;

    DetCNN quant conj cnn = {
      s = \\npf => cnn.s1 ! npf ++ conj.s2 ++ cnn.s2 ! npf ;
      a = agrP3 (conjNumber cnn.n conj.n) ;
      isPron = False ;
      isNeg = cnn.isNeg
      } ;

    ReflPossCNN conj cnn = {
      s = \\agr,npf => cnn.r1 ! agr ! npf ++ conj.s2 ++ cnn.r2 ! agr ! npf ;
      isPron = False ;
      isNeg = cnn.isNeg
      } ;

    PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,npf => cnn.r1 ! agr ! npf ++ conj.s2 ++ cnn.r2 ! agr ! npf ++ rnp.s ! agr ! NPCase Gen ;
      isPron = False ;
      isNeg = orB cnn.isNeg rnp.isNeg
      } ;

    NumLess num = num ** {
      s = \\n,c => num.s ! n ! c ++ "vähemmän" ;
      isNum = True
      } ;

    NumMore num = num ** {
      s = \\n,c => num.s ! n ! c ++ "lisää" ;
      isNum = True
      } ;

    UseACard acard = {
      s = acard.s ;
      n = acard.n
      } ;

    UseAdAACard ada acard = {
      s = \\n,c => ada.s ++ (acard.s ! n ! c) ;
      n = acard.n
      } ;

    ComparAdv pol cadv adv comp = {
      s = pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! agrP3 Sg
      } ;

    CAdvAP pol cadv ap comp = ap ** {
      s = \\isMod,nf => pol.s ++ cadv.s ++ ap.s ! isMod ! nf ++ cadv.p ++ comp.s ! agrP3 Sg ;
      hasPrefix = False
      } ;

    AdnCAdv pol cadv = {
      s = pol.s ++ cadv.s ++ cadv.p
      } ;

    EnoughAP ap ant pol vp = ap ** {
      s = \\isMod,nf => ap.s ! isMod ! nf ++ "kyllin" ++ infVPGen pol.p SCNom Pos (agrP3 Sg) vp Inf1 ;
      hasPrefix = False
      } ;

    EnoughAdv adv = {
      s = adv.s ++ "kyllin"
      } ;

    ExtAdvAP ap adv = ap ** {
      s = \\isMod,nf => ap.s ! isMod ! nf ++ SOFT_BIND ++ "," ++ adv.s ;
      hasPrefix = False
      } ;

    TimeNP np = {
      s = np.s ! NPSep
      } ;

    AdvAdv adv1 adv2 = {
      s = adv1.s ++ adv2.s
      } ;

    BareN2 n2 = n2 ;

    RecipVPSlash slash = G.ComplSlash slash recipNP ;

    RecipVPSlashCN slash cn = G.ComplSlash slash (reflPossNP (agrP3 Pl) G.NumSg cn) ;

    FocusComp comp np =
      mkClause (\_ -> comp.s ! np.a) np.a
        (insertObj (\\_,_,_ => np.s ! NPSep) (predV vpVerbOlla)) ;

    ReflVPSlash vps rnp =
      insertObjPre rnp.isNeg
        (\fin,b,agr -> appCompl fin b vps.c2 (rnp2np agr rnp))
        vps ;

    ReflA2 a rnp =
      let np = rnp2np (agrP3 Sg) rnp in {
        s = \\isMod,af =>
          preOrPost isMod (appCompl True Pos a.c2 np) (sAdjFull2nforms Posit a ! af) ;
        p = [] ;
        hasPrefix = False
        } ;

lin
  num x = x ;

lin RelNP = G.RelNP ;
    ExtRelNP = G.RelNP ;

lin whatSgFem_IP, whatSgNeut_IP = G.whatSg_IP ;

lin that_RP = G.IdRP ;

-- AR 2019-09-12
lin gen_Quant = S.a_Quant ;
----lin ReflVPSlash vpslash rnp =

}
