concrete ParseExtendSwe of ParseExtend = ParseCatSwe, ExtendSwe [A, N, VP, Tense, Adv, AdV, CompoundN, PositAdVAdj] ** open Prelude, ResSwe, CommonScand in {

lin ComplAV a vp = {
      s = \\ap => let agr = case ap of {
                              Strong (GSg g) => agrP3 g Sg ;
                              Strong GPl     => agrP3 Utr Pl ;
                              Weak   n       => agrP3 Utr n
                            }
                  in a.s ! AF (APosit ap) Nom ++ infMark ++ infVP vp agr ;
      isPre = False
    } ;

}
