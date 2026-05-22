concrete ParseExtendHun of ParseExtend =
  ExtendHun - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralHun - [num], PunctuationX **
  open Prelude, ResHun, NounHun, VerbHun, AdjectiveHun in {

lincat
  CNN = {s : Str ; n : Number ; agr : Person*Number ; g : Gender} ;

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
lin num x = x ;
lin NumMore num = num ** {s = \\p => "további" ++ num.s ! p} ;
lin NumLess num = num ** {s = \\p => num.s ! p ++ "kevesebb"} ;
lin UseACard card = card ;
lin UseAdAACard ada card = {s = \\p => ada.s ++ card.s ! p} ;
lin ExtRelNP = NounHun.RelNP ;
lin gen_Quant = DefArt ;
lin UttAP p ap =
      case p.agr of {
        <_,n> => {s = ap.s ! n ! Nom ++ ap.compl ! n}
        } ;
lin UttVPS p vps =
      ExtendHun.PredVPS (NounHun.UsePron p) vps ;
lin ReflVPSlash vps rnp = insertObj vps rnp ;
lin ReflA2 a2 rnp =
      let ap : AP = ComplA2 a2 rnp
       in ap ;
lin RelNP np rs = np ** {
	      s = \\p,c => np.s ! p ! c ++ rs.s ! np.g ! np.agr.p2 ! c
	      } ;
lin BareN2 n2 = n2 ;
lin BaseCNN num1 cn1 num2 cn2 =
      let n1 : Number = num2number num1.n ;
          n2 : Number = num2number num2.n ;
          det1 : Determiner = DetQuant (PossPron (pronTable ! <P3,Sg>)) num1 ;
          det2 : Determiner = DetQuant (PossPron (pronTable ! <P3,Sg>)) num2
       in {
        s = caseFromPossStem cn1 det1 Nom ++ cn1.compl ! n1 ! Nom
            ++ "és"
            ++ caseFromPossStem cn2 det2 Nom ++ cn2.compl ! n2 ! Nom ;
        n = Pl ;
        agr = <P3,Pl> ;
        g = cn2.g
        } ;
lin DetCNN quant conj cnn = indeclNP cnn.s ** {
      agr = cnn.agr ;
      objdef = Def ;
      g = cnn.g
      } ;
lin ReflPossCNN conj cnn = indeclNP cnn.s ** {
      agr = cnn.agr ;
      objdef = Def ;
      g = cnn.g
      } ;
lin PossCNN_RNP quant conj cnn rnp = indeclNP (cnn.s ++ rnp.s ! NoPoss ! Nom) ** {
      agr = cnn.agr ;
      objdef = Def ;
      g = cnn.g
      } ;
lin ExtAdvAP ap adv = ap ** {
	      compl = \\n => ap.compl ! n ++ bindComma ++ adv.s
	      } ;
lin ComparAdv pol cadv adv comp = {
      s = pol.s ++ if_then_Pol pol.p [] "nem" ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! VPres P3 Sg ;
      isPre = False
      } ;
lin CAdvAP pol cadv ap comp = ap ** {
      s = \\n,c => pol.s ++ if_then_Pol pol.p [] "nem" ++ cadv.s ++ ap.s ! n ! c ;
      compl = \\n => ap.compl ! n ++ cadv.p ++ comp.s ! VPres P3 n
      } ;
lin AdnCAdv pol cadv = {s = pol.s ++ if_then_Pol pol.p [] "nem" ++ cadv.s ++ cadv.p} ;
lin EnoughAP ap ant pol vp = ap ** {
      compl = \\n => ap.compl ! n ++ "eléggé" ++ infVP vp
      } ;
lin EnoughAdv adv = adv ** {s = adv.s ++ "eléggé"} ;
lin TimeNP np = {s = linNP np ; isPre = False} ;
lin AdvAdv adv1 adv2 = {
      s = adv1.s ++ adv2.s ;
      isPre = False
      } ;
lin whatSgFem_IP = indeclNP "mi" ;
lin whatSgNeut_IP = indeclNP "mi" ;
lin that_RP = {s = \\_,_,_ => "ami"} ;
lin EmbedVP ant pol p vp = {
      s = pol.s ++ if_then_Pol pol.p [] "nem" ++ infVP vp
      } ;
lin ComplVV vv ant pol vp =
      useV (vv ** {s = vv.s ! Indef}) ** {
        obj = vp.obj ;
        adv = pol.s ++ if_then_Pol pol.p [] "nem" ++ vp.adv ++ vp.s ! VInf
      } ;
lin SlashVV vv ant pol vps =
      vv ** {
        s = vv.s ;
        c2 = vps.c2 ;
        adv = pol.s ++ if_then_Pol pol.p [] "nem" ++ infVPSlash vps
      } ;
lin SlashV2V vv ant pol vp =
      useVc vv ** {
        adv = pol.s ++ if_then_Pol pol.p [] "nem" ++ infVP vp
      } ;
lin SlashV2VNP vv np ant pol vps =
      useVc vv ** {
        adv = np.s ! NoPoss ! Acc ++ pol.s ++ if_then_Pol pol.p [] "nem" ++ infVPSlash vps
      } ;
lin InOrderToVP ant pol p vp = {
      s = pol.s ++ if_then_Pol pol.p [] "nem" ++ infVP vp ++ "céljából" ;
      isPre = False
      } ;
lin CompVP ant pol p vp =
      useV (copula ** {s = \\vf => pol.s ++ if_then_Pol pol.p [] "nem" ++ infVP vp ++ copula.s ! vf}) ;
lin FocusComp comp np = predVP np comp ;
lin UttVP ant pol p vp = {
      s = pol.s ++ if_then_Pol pol.p [] "nem" ++ infVP vp
      } ;
lin RecipVPSlash slash = insertObj slash (indeclNP "egymást" ** {objdef = Def}) ;
lin RecipVPSlashCN slash cn = insertObj slash (DetCN (DetQuant DefArt NumSg) cn) ;

}
