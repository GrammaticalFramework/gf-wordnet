concrete ParseExtendIna of ParseExtend =
   ExtendIna - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron,
                theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
                N2VPSlash, A2VPSlash, CompVP, InOrderToVP, PurposeVP,
                ComplGenVV, ReflRNP, ReflA2RNP, UncontractedNeg,
                AdvIsNPAP, ExistCN, NominalizeVPSlashNP,
                PiedPipingQuestSlash, PiedPipingRelSlash],
   NumeralIna - [num], PunctuationX **
open Prelude, ResIna, GrammarIna in {

lin
  PhrUttMark pconj utt voc mark =
    {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;
  gen_Quant = {s = \\_,c => casePrep [] c} ;

  UttAP pron ap = {s = ap.s ! pron.a} ;
  UttVPS pron vps = {s = vps.s ! pron.a} ;

  ReflA2 a rnp = {
    s = \\agr => a.s ! AAdj Posit ++ a.c2 ++ rnp.s ! agr ! Acc ;
    isPre = False
    } ;
  ReflVPSlash vps rnp = insertReflObj vps.p2 vps.c2 rnp vps ;

lincat
  CNN = {s1,s2 : Str ; n : Number} ;

lin
  BaseCNN n1 cn1 n2 cn2 = {
    s1 = cn1.s ! n1.n ; s2 = cn2.s ! n2.n ; n = Pl
    } ;
  DetCNN quant conj cnn = {
    s = \\c => quant.s ! cnn.n ! c ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ;
    a = agrP3 cnn.n ;
    isPronoun = False
    } ;
  ReflPossCNN conj cnn = {
    s = \\a,c => casePrep [] c ++ reflPossStr a ++
                   cnn.s1 ++ conj.s2 ++ cnn.s2 ;
    isPronoun = False
    } ;
  PossCNN_RNP quant conj cnn rnp = {
    s = \\a,c => quant.s ! cnn.n ! c ++ cnn.s1 ++ conj.s2 ++ cnn.s2 ++
                   rnp.s ! a ! Gen ;
    isPronoun = False
    } ;

  NumLess num = num ** {s = num.s ++ "minus"} ;
  NumMore num = num ** {s = num.s ++ "plus"} ;
  num n = n ;
  UseACard card = {s = card.s ; n = Pl} ;
  UseAdAACard ada card = {s = ada.s ++ card.s ; n = Pl} ;

  RelNP np rs = {
    s = \\c => np.s ! c ++ rs.s ! np.a ;
    a = np.a ;
    isPronoun = np.isPronoun
    } ;
  ExtRelNP np rs = {
    s = \\c => np.s ! c ++ "," ++ rs.s ! np.a ;
    a = np.a ;
    isPronoun = np.isPronoun
    } ;
  ExtAdvAP ap adv = {
    s = \\a => ap.s ! a ++ "," ++ adv.s ;
    isPre = False
    } ;
  BareN2 n = {s = n.s} ;

  ComparAdv pol cadv adv comp = {
    s = posneg pol.p ++ cadv.s ++ adv.s ++ cadv.p ++ comp.s ! Sp3
    } ;
  CAdvAP pol cadv ap comp = {
    s = \\a => posneg pol.p ++ cadv.s ++ ap.s ! a ++ cadv.p ++ comp.s ! a ;
    isPre = False
    } ;
  AdnCAdv pol cadv = {s = posneg pol.p ++ cadv.s ++ cadv.p} ;

  EnoughAP ap ant pol vp = {
    s = \\a => ap.s ! a ++ "satis pro" ++ infAnt ant.a pol.p vp a ;
    isPre = False
    } ;
  EnoughAdv adv = {s = adv.s ++ "satis"} ;
  TimeNP np = {s = np.s ! Acc} ;
  AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

  whatSgFem_IP = {s = \\c => casePrep [] c ++ "que" ; n = Sg} ;
  whatSgNeut_IP = {s = \\c => casePrep [] c ++ "que" ; n = Sg} ;
  that_RP = IdRP ;

  EmbedVP ant pol pron vp = {s = infAnt ant.a pol.p vp pron.a} ;
  ComplVV vv ant pol vp =
    insertInvarObj (infAnt ant.a pol.p vp Sp3) (predV vv) ;
  SlashVV vv ant pol vps =
    insertInvarObj (infAnt ant.a pol.p (lin VP vps) Sp3) (predV vv) **
      {p2 = vps.p2 ; c2 = vps.c2} ;
  SlashV2V v2v ant pol vp =
    insertInvarObj (infAnt ant.a pol.p vp Sp3) (predV v2v) **
      {p2 = v2v.p2 ; c2 = v2v.c2} ;
  SlashV2VNP v2v np ant pol vps =
    insertObj v2v.p2 v2v.c2 np
      (insertInvarObj (infAnt ant.a pol.p (lin VP vps) Sp3) (predV v2v)) **
      {p2 = vps.p2 ; c2 = vps.c2} ;
  InOrderToVP ant pol pron vp =
    {s = "pro" ++ infAnt ant.a pol.p vp pron.a} ;
  CompVP ant pol pron vp =
    {s = \\_ => infAnt ant.a pol.p vp pron.a} ;
  UttVP ant pol pron vp = {s = infAnt ant.a pol.p vp pron.a} ;

  RecipVPSlash vps =
    insertObj vps.p2 vps.c2 (mkInvarNP "le unes le alteres") vps ;
  RecipVPSlashCN vps cn =
    insertObj vps.p2 vps.c2
      (mkInvarNP (cn.s ! Pl ++ "le unes del alteres")) vps ;

  FocusComp comp np = PredVP np (UseComp comp) ;

oper
  infAnt : Anteriority -> Polarity -> ResIna.VP -> Agr -> Str = \ant,pol,vp,a ->
    posneg pol ++ case ant of {
      Simul => variants {
        vp.clitics ! a ++ vp.inf ++ vp.rest ! a ;
        vp.inf ++ vp.clitics ! a ++ vp.rest ! a
        } ;
      Anter => "haber" ++ vp.part ++ vp.clitics ! a ++ vp.rest ! a
      } ;

  reflPossStr : Agr -> Str = \a -> case <a.n,a.p> of {
    <Sg,P1> => "mi" ; <Sg,P2> => "tu" ; <Sg,P3> => "su" ;
    <Pl,P1> => "nostre" ; <Pl,P2> => "vostre" ; <Pl,P3> => "lor"
    } ;
}
