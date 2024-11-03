concrete ParseExtendIce of ParseExtend =
  NumeralIce - [num], PunctuationX ** 
  open GrammarIce, Prelude in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

lin that_RP = IdRP ;

lin num x = {
	  s = \\ngc =>  x.s ! ngc ;
      n = sizeToNumber x.size
    } ;

}
