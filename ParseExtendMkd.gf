concrete ParseExtendMkd of ParseExtend =
  ExtendMkd - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],  NumeralMkd - [num], PunctuationX ** open Predef, Prelude, ResMkd, GrammarMkd in {

lincat CNN = {s1,s2 : Species => Str; g1,g2 : Gender; n1,n2 : NNumber} ;

oper
  cnWithNum : Num -> CN -> Species -> Str =
    \num,cn,sp -> case num.n of {
      NNum n => num.s ++ cn.s ! sp ! n ;
      NCountable => num.s ++ cn.count_form
    } ;

  daVP : Ant -> Pol -> Agr -> VP -> Str =
    \ant,pol,agr,vp ->
      ant.s ++ pol.s ++ "да" ++
      case pol.p of {
        Pos => [] ;
        Neg => "не"
      } ++
      case ant.a of {
        Simul => vp.present ! Perfective ! genNum2num agr.g ! agr.p ++ vp.compl ! agr ;
        Anter => auxBe.present ! genNum2num agr.g ! agr.p ++
                 vp.participle.imperfect ! Perfective ! agr.g ++ vp.compl ! agr
      } ;

lin gen_Quant = DefArt ;
lin that_RP = IdRP ;
lin UttAP  p ap  = {s = ap.s ! Indef ! p.a.g} ;
lin UttVPS p vps = {s = vps.s ! p.a} ;
lin UttVP ant pol p vp = {
      s = daVP ant pol p.a vp
      } ;
lin EmbedVP ant pol p vp = {
      s = daVP ant pol p.a vp
      } ;
lin CompVP ant pol p vp = {
      s = \\_ => daVP ant pol p.a vp
    } ;
lin PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin num x = x ;

lin BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\sp => cnWithNum num1 cn1 sp ;
      s2 = \\sp => cnWithNum num2 cn2 sp ;
      g1 = cn1.g ;
      g2 = cn2.g ;
      n1 = num1.n ;
      n2 = num2.n
    } ;

lin DetCNN quant conj cnn = {
      s = \\_ => quant.s ! genNum cnn.g1 (nnum2num cnn.n1) ++
                 cnn.s1 ! quant.sp ++ conj.s ++
                 quant.s ! genNum cnn.g2 (nnum2num cnn.n2) ++
                 cnn.s2 ! quant.sp ;
      vocative = quant.s ! genNum cnn.g1 (nnum2num cnn.n1) ++
                 cnn.s1 ! quant.sp ++ conj.s ++
                 quant.s ! genNum cnn.g2 (nnum2num cnn.n2) ++
                 cnn.s2 ! quant.sp ;
      a = {g = GPl; p = P3}
    } ;

lin ReflPossCNN conj cnn = {
      s = \\_,_ => ReflPossPron.s ! genNum cnn.g1 (nnum2num cnn.n1) ++
                   cnn.s1 ! Indef ++ conj.s ++
                   ReflPossPron.s ! genNum cnn.g2 (nnum2num cnn.n2) ++
                   cnn.s2 ! Indef ;
      a = {g = GPl; p = P3} ;
      isPron = False
    } ;

lin PossCNN_RNP quant conj cnn rnp = {
      s = \\agr,_ => quant.s ! genNum cnn.g1 (nnum2num cnn.n1) ++
                     cnn.s1 ! quant.sp ++ conj.s ++
                     quant.s ! genNum cnn.g2 (nnum2num cnn.n2) ++
                     cnn.s2 ! quant.sp ++ "на" ++ rnp.s ! agr ! RObj Acc ;
      a = {g = GPl; p = P3} ;
      isPron = False
    } ;

lin NumMore num = {s = num.s ++ "повеќе"; n = NNum Pl} ;
lin NumLess num = {s = num.s ++ "помалку"; n = NNum Pl} ;
lin UseACard card = {s = card.s} ;
lin UseAdAACard ada card = {s = ada.s ++ card.s} ;

lin RelNP np rs = {
      s = \\r => np.s ! r ++ rs.s ! np.a.g ;
      vocative = np.vocative ++ rs.s ! np.a.g ;
      a = np.a
    } ;
lin ExtRelNP = RelNP ;
lin BareN2 n2 = n2 ;

lin ComparAdv pol cadv adv comp = {
      s = pol.s ++
          case pol.p of {Pos => []; Neg => "не"} ++
          cadv.s ++ adv.s ++ cadv.p ++ comp.s ! GSg Masc
    } ;
lin CAdvAP pol cadv ap comp = {
      s = \\sp,gn => pol.s ++
                     case pol.p of {Pos => []; Neg => "не"} ++
                     cadv.s ++ ap.s ! sp ! gn ++ cadv.p ++ comp.s ! gn ;
      isPre = False
    } ;
lin AdnCAdv pol cadv = {
      s = pol.s ++ case pol.p of {Pos => []; Neg => "не"} ++ cadv.s ++ cadv.p
    } ;
lin EnoughAP ap ant pol vp = {
      s = \\sp,gn => ap.s ! sp ! gn ++ "доволно" ++
                     daVP ant pol {g = gn; p = P3} vp ;
      isPre = False
    } ;
lin EnoughAdv adv = {s = adv.s ++ "доволно"} ;
lin ExtAdvAP ap adv = {
      s = \\sp,gn => ap.s ! sp ! gn ++ SOFT_BIND ++ "," ++ adv.s ;
      isPre = False
    } ;
lin AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;
lin whatSgFem_IP = {s = "која"; g = GSg Fem} ;
lin whatSgNeut_IP = {s = "што"; g = GSg Neuter} ;

lin ComplVV vv ant pol vp = vv ** {
      compl = \\agr => daVP ant pol agr vp ;
    } ;

lin SlashVV vv ant pol slash = vv ** {
      compl = \\agr => daVP ant pol agr (lin VP slash) ;
      c2 = slash.c2
    } ;
lin SlashV2V v2v ant pol vp = v2v ** {
      compl = \\agr => daVP ant pol agr vp
    } ;
lin SlashV2VNP v2v np ant pol slash = v2v ** {
      compl = \\agr => v2v.c2.s ++ np.s ! RObj v2v.c2.c ++
                       daVP ant pol np.a (lin VP slash) ;
      c2 = slash.c2
    } ;

lin InOrderToVP ant pol p vp = {
      s = "за" ++ daVP ant pol p.a vp
    } ;

lin ReflA2 a2 rnp = {
      s = \\sp,gn => a2.s ! sp ! gn ++ a2.c2.s ++
                     rnp.s ! {g = gn; p = P3} ! RObj a2.c2.c ;
      isPre = False
    } ;

lin ReflVPSlash slash rnp =
      slash ** {
        compl = \\agr =>
          slash.compl ! agr ++
          case <rnp.isPron,slash.c2.c> of {
            <True,Acc> => [] ;
            <True,Dat> => [] ;
            _ => slash.c2.s ++ rnp.s ! agr ! RObj slash.c2.c
          } ;
        vtype = case <rnp.isPron,slash.c2.c> of {
          <True,Acc> => VMedial Acc ;
          <True,Dat> => VMedial Dat ;
          _ => slash.vtype
        }
      } ;

lin RecipVPSlash slash =
      slash ** {compl = \\agr => slash.compl ! agr ++ slash.c2.s ++ "еден со друг"} ;
lin RecipVPSlashCN slash cn =
      slash ** {compl = \\agr => slash.compl ! agr ++ slash.c2.s ++
                              "еден" ++ cn.s ! Indef ! Sg ++ "со друг"} ;

lin FocusComp comp np = PredVP np (UseComp comp) ;

lin TimeNP np = {s = np.s ! RSubj} ;

}
