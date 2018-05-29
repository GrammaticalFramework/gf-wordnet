concrete ParseExtendEng of ParseExtend = ParseCatEng, ExtendEng [A, N, VP, Tense, Adv, AdV, CompoundN, PositAdVAdj] ** open Prelude, ResEng in {

lin ComplAV a vp = {
      s = \\agr => a.s ! AAdj Posit Nom ++ infVP VVInf vp Simul CPos agr ;
      isPre = False
    } ;

}
