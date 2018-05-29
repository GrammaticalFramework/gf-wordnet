concrete ParseExtendBul of ParseExtend = ParseCatBul, ExtendBul [A, N, VP, Tense, Adv, AdV, CompoundN, PositAdVAdj] ** open Prelude, ResBul in {

lin ComplAV a vp = {
      s = \\aform => let agr = agrP3 (aform2gennum aform) ;
                     in a.s ! aform ++ daComplex Simul Pos vp ! Perf ! agr ;
      adv = a.adv ++ daComplex Simul Pos vp ! Perf ! agrP3 (GSg Neut) ;
      isPre = False
    } ;

}
