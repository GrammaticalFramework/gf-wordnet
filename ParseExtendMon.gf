concrete ParseExtendMon of ParseExtend =
  ExtendMon - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP, N2VPSlash, A2VPSlash,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
               PiedPipingQuestSlash, PiedPipingRelSlash],
  NumeralMon - [num], PunctuationX **
  open Prelude, ResMon, ParadigmsMon, (G = GrammarMon), (E = ExtendMon) in {

lincat CNN = {s1, s2 : RCase => Str ; n : Number} ;

oper
  infVPPol : VerbPhrase -> Anteriority -> Polarity -> RCase -> Str = \vp,ant,pol,rc ->
    let args = vp.compl ! rc ++ vp.embedCompl ! rc ++ vp.adv
    in case ant of {
      Simul => args ++ (vp.s ! VPPart ClFut pol rc).fin ;
      Anter => case pol of {
        Pos => args ++ (vp.s ! VPPart (ClPast Perf) Pos Nom).fin ++ "байх" ;
        Neg => args ++ (vp.s ! VPPart ClPres Neg Nom).fin ++ "байх"
        }
      } ;

lin
 gen_Quant = {
   s = \\_ => [] ; sp = \\_,_ => [] ; isPoss = False ; isDef = False
   } ;

 UttAP pron ap = {s = pron.s ! PronCase Nom ++ ap.s} ;
 UttVPS pron vps = {s = pron.s ! PronCase Nom ++ vps.s} ;

 PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

 BareN2 n = n ;
 RelNP np rs = np ** {s = \\rc => rs.s ! Attributive ++ np.s ! rc} ;
 ExtRelNP = G.RelNP ;
 ExtAdvAP ap adv = {s = ap.s ++ "," ++ adv.s} ;

 BaseCNN n1 cn1 n2 cn2 = {
   s1 = \\rc => cn1.s ! n1.n ! toNCase rc Definite ;
   s2 = \\rc => cn2.s ! n2.n ! toNCase rc Definite ;
   n = Pl
   } ;
 DetCNN quant conj cnn = {
   s = \\rc => quant.s ! cnn.n ++ cnn.s1 ! rc ++ conj.s2 ++ cnn.s2 ! rc ;
   n = cnn.n ; p = P3 ; isPron = False ; isDef = quant.isDef
   } ;
 ReflPossCNN conj cnn = lin RNP {
   s = \\rc => reflPron ! Sg ! Gen ++ cnn.s1 ! rc ++ conj.s2 ++ cnn.s2 ! rc ;
   n = cnn.n ; p = P3 ; isPron = False ; isDef = True ; lock_NP = <>
   } ;
 PossCNN_RNP quant conj cnn rnp = lin RNP {
   s = \\rc => quant.s ! cnn.n ++ cnn.s1 ! rc ++ conj.s2 ++ cnn.s2 ! rc ++ rnp.s ! rc ;
   n = cnn.n ; p = P3 ; isPron = False ; isDef = quant.isDef ; lock_NP = <>
   } ;

 ReflVPSlash = E.ReflRNP ;
 ReflA2 = E.ReflA2RNP ;

 NumLess n = n ** {s = "дутуу" ++ n.s ; sp = \\rc => "дутуу" ++ n.sp ! rc} ;
 NumMore n = n ** {s = n.s ++ "илүү" ; sp = \\rc => n.sp ! rc ++ "илүү"} ;
 UseACard card = {s = card.s ; sp = \\_ => card.s ; n = Pl} ;
 UseAdAACard ada card = {s = ada.s ++ card.s ; sp = \\_ => ada.s ++ card.s ; n = Pl} ;

 ComparAdv pol cadv adv comp = {
   s = pol.s ++ comp.s ! cadv.c2.rc ++ cadv.c2.s ++ cadv.s ++ adv.s
   } ;
 CAdvAP pol cadv ap comp = {
   s = pol.s ++ comp.s ! cadv.c2.rc ++ cadv.c2.s ++ cadv.s ++ ap.s
   } ;
 AdnCAdv pol cadv = {s = pol.s ++ cadv.s ++ cadv.c2.s} ;
 EnoughAP ap ant pol vp = {s = infVPPol vp ant.a pol.p Nom ++ "хангалттай" ++ ap.s} ;
 EnoughAdv adv = {s = "хангалттай" ++ adv.s} ;
 TimeNP np = {s = np.s ! Nom} ;
 AdvAdv adv mod = {s = mod.s ++ adv.s} ;

 whatSgFem_IP = {s = \\rc => (regN "юу").s ! SF Sg (toNCase rc Definite)} ;
 whatSgNeut_IP = whatSgFem_IP ;
 that_RP = {s = []} ;

 EmbedVP ant pol pron vp = {s = pron.s ! PronCase Acc ++ infVPPol vp ant.a pol.p Nom ++ "нь"} ;
 ComplVV vv ant pol vp = insertObj (\\_ => infVPPol vp ant.a pol.p Nom) (predV (auxToVerb vv)) ;
 SlashVV vv ant pol slash =
   insertObj (\\_ => infVPPol slash ant.a pol.p Nom) (predV (auxToVerb vv)) ** {c2 = slash.c2} ;
 SlashV2V v ant pol vp =
   insertObj (\\_ => infVPPol vp ant.a pol.p Nom) (predV v) ** {c2 = v.c2} ;
 SlashV2VNP v np ant pol slash =
   insertObj (\\_ => appCompl v.c2 np.s ++ infVPPol slash ant.a pol.p Nom) (predV v) ** {c2 = slash.c2} ;
 InOrderToVP ant pol pron vp = {s = pron.s ! PronCase Acc ++ infVPPol vp ant.a pol.p Dat ++ "тулд"} ;
 CompVP ant pol pron vp = {s = \\_ => pron.s ! PronCase Acc ++ infVPPol vp ant.a pol.p Nom} ;
 UttVP ant pol pron vp = {s = pron.s ! PronCase Nom ++ infVPPol vp ant.a pol.p Nom} ;

 RecipVPSlash slash = insertObj (\\_ => "бие биенээ") slash ;
 RecipVPSlashCN slash cn = insertObj (\\_ => cn.s ! Pl ! NAcc Definite) slash ;
 FocusComp comp np = {s = \\_,_,_,_ => comp.s ! Nom ++ np.s ! Nom} ;

 num n = {s = n.s ; n = n.n} ;

}
