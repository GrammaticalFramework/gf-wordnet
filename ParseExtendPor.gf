concrete ParseExtendPor of ParseExtend =
  ExtendPor - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP], NumeralPor - [num] **
  open Prelude, ResPor, MorphoPor, GrammarPor, (E = ExtraPor), Coordination in {

  lin
    --^
    gen_Quant = {
      s = \\b,n,g,c => "" ;
      s2 = "" ;
      sp = \\n,g,c => artDef False g n c ;
      isNeg = False
      } ;

    UttAP        = UttAPMasc ;
    UttAPMasc ap = {s = ap.s ! AF Masc Sg} ;
    UttAPFem  ap = {s = ap.s ! AF Fem Sg} ;

    UttVPS = UttVPSMasc ;
    UttVPSMasc vps = {s = vps.s ! Ag Masc Sg P3} ;
    UttVPSFem vps = {s = vps.s ! Ag Fem Sg P3} ;
    UttVPSPl vps = {s = vps.s ! Ag Masc Pl P3} ;

  lincat Mark = SS ;

  lin
    PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ BIND ++ mark.s} ;
    FullStop  = {s = "."} ;
    ExclMark  = {s = "!"} ;
    QuestMark = {s = "?"} ;

  --^ RNP is not implemented yet in ExtendPor
  --lin AdvRNP np prep rnp = rnp ;

  lincat
    -- CN = {s : Number => Str ; g : Gender}
    -- True if digit
    CNN = {s1 : Bool => Str ; s2 : Str; n1,n : Number; g1 : Gender} ;
  lin
    --     Num     = {s : Gender => Str ; isNum : Bool ; n : Number} ;
    BaseCNN num1 cn1 num2 cn2 = {
      s1 = \\d => num1.s ! cn1.g ++ cn1.s ! num1.n ;
      s2 = num2.s ! cn2.g ++ cn2.s ! num2.n ;
      n1 = num1.n ;
      g1 = cn1.g ;
      n = conjNumber num1.n num2.n ;
      } ;

  lin
    NumLess n = {s = \\g => n.s ! g ++ "menos" ;
                 n = n.n ; -- shouldn't it be 2 - ?
                 isNum = n.isNum} ;

    NumMore n = {s = \\g => n.s ! g ++ "mais" ;
                 n = n.n ; isNum = n.isNum } ;

  lincat Sub1000000000 = {s : CardOrd => Str ; n : Number} ;

  ---- Numeral = {s : CardOrd => Str ; n : Number} ;
  ---- CardOrd = NCard Gender | NOrd Gender Number ;
  lin
    pot3as4 n = n ;
    pot4 n = {s = \\g => n.s ! NCard Masc ++ milhao g; n = Pl} ;
    pot4plus n m = {s = \\g => n.s! NCard Masc ++ milhao g ++ e_CardOrd g ++ m.s ! g ;
                    n = Pl
      } ;

    pot21 = mkNum "cem" "centésimo" "centésima" ;
    pot31 = mkNum "mil" "milésimo" "milésima" ;
    pot41 = mkNum "milhão" "milhonésimo" "milhonésima" ;
  oper
    milhao : CardOrd -> Str = \g ->
      (mkTal "milhão" [] [] [] "milhonésimo" [] []).s ! unit ! g ;

    mkNum : Str -> Str -> Str -> {s : CardOrd => Str ; n : Number} ;
    -- probably temporary, NumeralPor needs refactoring
    mkNum cem centesimo centesima =
      {s = \\g =>
         case g of {
           NCard _     => cem ;
           NOrd Masc _ => centesimo ;
           NOrd Fem  _ => centesima
         } ;
       n = Pl } ;

  lin
    num x = x ;

  lin
    BareN2 n2 = n2 ;

  lin
    AdvAdv adv1 adv2 = {s = adv1.s ++ adv2.s} ;

  lin
    AdvImp adv imp = {
      s = \\pol,impform,g => imp.s ! pol ! impform ! g ++ adv.s
      } ;

  lin
    whatSgFem_IP = whatSg_IP ** {a = aagr Fem Sg} ;
    whatSgNeut_IP = whatSg_IP ;

} ;
