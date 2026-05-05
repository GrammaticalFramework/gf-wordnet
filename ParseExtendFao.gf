concrete ParseExtendFao of ParseExtend =
  ExtendFao - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron], PunctuationX ** open Prelude, ResFao in {

lin UttAP  p ap  = {s = ap.s ! p.g ! p.n ! Nom} ;
    PhrUttMark pconj utt voc mark = {s = CAPIT ++ pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin num x = x ;

}
