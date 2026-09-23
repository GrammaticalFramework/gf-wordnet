--# -path=.:../gf-rgl/src/russian:../gf-rgl/src/api:../gf-rgl/src/abstract:../gf-rgl/src/common:prelude
concrete ParseExtendRus of ParseExtend =
  ExtendRus - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash], NumeralRus - [num], PunctuationX **
  open ParamRus, GrammarRus, ParadigmsRus, ResRus, Coordination, Prelude in {

lincat CNN = {
  s1,s2 : Case => Str ;
  g : Gender ;
  anim : Animacy ;
  n1,n2 : NumSize
  } ;

lin UttAP p ap = {s = case p.a of {
                         Ag gn p => ap.s ! gn ! Animate ! Nom
                      }
                 } ;

    UttVPS p vps = {
      s=p.nom ++ vps.s ! Ind ! p.a
      } ;

    UttVP ant pol p vp = {
      s=ant.s ++ pol.s ++
        case pol.p of {
          Neg => "не" ;
          Pos => ""
        } ++
        vp.adv ! p.a ++ (verbInf vp.verb) ++ vp.dep ++ vp.compl ! Pos ! p.a
      } ;

lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

  -- : VV -> VP -> VP ;  -- want to run
lin ComplVV vv ant pol vp = vp ** {
      verb=vv.v ;
      dep=ant.s ++ pol.s ++ verbInf vp.verb ++ vp.dep ;
      adv=\\a=>vv.modal ! a ++ vp.adv ! a ;
      p = Pos
    } ;

lin SlashV2V v2v ant pol vp = insertSlashObj2 (\\_,a => ant.s ++ pol.s ++ verbInf vp.verb) v2v.c (slashV v2v v2v.c) ;

lin num x = x ;

lin gen_Quant = {
      s=\\gn,anim,cas=>[] ;
      type=EmptyIndef ;
      short=\\a=>[] ;
      c=Nom ;
      preferShort=PreferFull
    } ;

lin that_RP = IdRP ;

lin RelNP = GrammarRus.RelNP ;
    ExtRelNP = GrammarRus.RelNP ;

lin ReflA2 a2 rnp = {
      s=\\gn,anim,cas =>
        (adjFormsAdjective a2).s ! gn ! anim ! cas
        ++ a2.c.s ++ rnp.s ! a2.c.c ;
      short=\\a =>
        (adjFormsAdjective a2).short ! a
        ++ a2.c.s ++ rnp.s ! a2.c.c ;
      isPost=False ;
      preferShort=a2.preferShort
    } ;

    ReflVPSlash vps rnp = {
      verb=vps.verb ;
      adv=vps.adv ;
      dep=vps.dep ;
      compl=\\p,a => vps.compl1 ! p ! a ++ vps.c.s
        ++ rnp.s ! vps.c.c ++ vps.compl2 ! p ! a ;
      p=vps.p
    } ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1=\\cas => num1.s ! cn1.g ! cn1.anim ! cas
        ++ cn1.s ! animNumSizeNum cn1.anim cas num1.size
                 ! numSizeCase cas num1.size ;
      s2=\\cas => num2.s ! cn2.g ! cn2.anim ! cas
        ++ cn2.s ! animNumSizeNum cn2.anim cas num2.size
                 ! numSizeCase cas num2.size ;
      g=cn2.g ; anim=cn2.anim ; n1=num1.size ; n2=num2.size
    } ;

    DetCNN quant conj cnn = {
      s=\\cas => quant.s ! gennum cnn.g (numSizeNumber cnn.n1)
                         ! cnn.anim ! cas
        ++ cnn.s1 ! cas ++ conj.s1 ++ conj.s2 ++ cnn.s2 ! cas ;
      pron=False ;
      a=Ag (gennum cnn.g Pl) P3
    } ;

    ReflPossCNN conj cnn = {
      s=\\cas => (mkPronTable (reflexivePron (Ag (GSg Masc) P3)).poss)
                    ! gennum cnn.g Pl ! cnn.anim ! cas
        ++ cnn.s1 ! cas ++ conj.s1 ++ conj.s2 ++ cnn.s2 ! cas
    } ;

    PossCNN_RNP quant conj cnn rnp = {
      s=\\cas => quant.s ! gennum cnn.g Pl ! cnn.anim ! cas
        ++ cnn.s1 ! cas ++ conj.s1 ++ conj.s2 ++ cnn.s2 ! cas
        ++ rnp.s ! Gen
    } ;

lin NumLess n = n ;
    NumMore n = n ;

    UseACard card = {
      s=\\_,_,_ => card.s ;
      size=Num5
    } ;
    UseAdAACard ada card = {
      s=\\_,_,_ => ada.s ++ card.s ;
      size=Num5
    } ;

    ExtAdvAP ap adv = ap ** {
      s=\\gn,anim,cas => ap.s ! gn ! anim ! cas ++ comma ++ adv.s ;
      short=\\a => ap.short ! a ++ comma ++ adv.s ;
      isPost=True
    } ;

    BareN2 n = n ;

    ComparAdv pol cadv adv comp = {
      s=pol.s ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! Ag (GSg Neut) P3
    } ;

    CAdvAP pol cadv ap comp = ap ** {
      s=\\gn,anim,cas => pol.s ++ cadv.s ++ ap.s ! gn ! anim ! cas
        ++ cadv.p ++ comp.s ! genNumAgrP3 gn ;
      short=\\gn => pol.s ++ cadv.s ++ ap.short ! gn
        ++ cadv.p ++ comp.s ! genNumAgrP3 gn
    } ;

    AdnCAdv pol cadv = {s=pol.s ++ cadv.s ++ cadv.p} ;

    EnoughAP ap ant pol vp = ap ** {
      s=\\gn,anim,cas => ap.s ! gn ! anim ! cas ++ "достаточно"
        ++ pol.s ++ verbInf vp.verb ++ vp.dep
        ++ vp.compl ! pol.p ! genNumAgrP3 gn ;
      short=\\gn => ap.short ! gn ++ "достаточно" ++ pol.s
        ++ verbInf vp.verb ++ vp.dep ++ vp.compl ! pol.p ! genNumAgrP3 gn ;
      isPost=True
    } ;

    EnoughAdv adv = {s="достаточно" ++ adv.s} ;
    TimeNP np = {s=np.s ! Acc} ;
    AdvAdv adv1 adv2 = {s=adv1.s ++ adv2.s} ;

    whatSgFem_IP = {
      nom="что"; gen="чего"; dat="чему"; acc="что"; ins="чем"; prep="чём";
      poss=(reflexivePron (Ag (GSg Fem) P3)).poss;
      anim=Inanimate; a=Ag (GSg Fem) P3
    } ;
    whatSgNeut_IP = {
      nom="что"; gen="чего"; dat="чему"; acc="что"; ins="чем"; prep="чём";
      poss=(reflexivePron (Ag (GSg Neut) P3)).poss;
      anim=Inanimate; a=Ag (GSg Neut) P3
    } ;

lin EmbedVP ant pol p vp = {
      s=ant.s ++ pol.s
        ++ vp.adv ! p.a
        ++ (verbInf vp.verb)
        ++ vp.dep
        ++ vp.compl ! Pos ! p.a    -- ???
    } ;

    SlashVV vv ant pol vps = vps ** {
      verb=vv.v ;
      dep=ant.s ++ pol.s ++ verbInf vps.verb ++ vps.dep ;
      adv=\\a => vv.modal ! a ++ vps.adv ! a ;
      p=pol.p
    } ;

    SlashV2VNP v2v np ant pol vps =
      insertSlashObj1
        (\\p,a => applyPolPrep p v2v.c np ++ ant.s ++ pol.s
          ++ verbInf vps.verb ++ vps.dep
          ++ vps.compl1 ! p ! a ++ vps.compl2 ! p ! a)
        vps.c (slashV v2v vps.c) ;

    InOrderToVP ant pol p vp = {
      s="чтобы" ++ ant.s ++ pol.s ++ vp.adv ! p.a
        ++ verbInf vp.verb ++ vp.dep ++ vp.compl ! pol.p ! p.a
    } ;

    CompVP ant pol p vp = {
      s=\\a => ant.s ++ pol.s ++ vp.adv ! p.a
        ++ verbInf vp.verb ++ vp.dep ++ vp.compl ! pol.p ! p.a ;
      adv=[] ; cop=EllCopula
    } ;

    RecipVPSlash vps = {
      verb=vps.verb ; adv=vps.adv ; dep=vps.dep ;
      compl=\\p,a => vps.compl1 ! p ! a ++ vps.c.s
        ++ "друг друга" ++ vps.compl2 ! p ! a ;
      p=vps.p
    } ;

    RecipVPSlashCN vps cn = {
      verb=vps.verb ; adv=vps.adv ; dep=vps.dep ;
      compl=\\p,a => vps.compl1 ! p ! a ++ vps.c.s
        ++ cn.s ! Sg ! vps.c.c ++ vps.compl2 ! p ! a ;
      p=vps.p
    } ;

    FocusComp comp np = {
      subj=comp.adv ++ comp.s ! np.a ;
      adv=[] ; verb=selectCopula comp.cop ; dep=[] ;
      compl=\\_ => np.s ! Nom ;
      a=np.a
    } ;

}
