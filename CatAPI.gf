concrete CatAPI of Cat = open Prelude in {

lincat 
  A, A2, AP, AdA, AdN, AdV, Adv, Ant, CAdv, CN, Card, Cl, ClSlash, Comp, Conj, Det, Digits, Decimal,
  IAdv, IComp, IDet, IP, IQuant, Imp, Interj, N, N2, N3, NP, Num, Numeral, 
  Ord, PConj, GN, SN, PN, Phr, Pol, Predet, Prep, Pron, QCl, QS, Quant, RCl, RP, RS, S, SC, SSlash, 
  Subj, Temp, Tense, Text, Utt, V, V2, V2A, V2Q, V2S, V2V, V3, 
  VA, VP, VPSlash, VQ, VS, VV, Voc, ACard, DAP, LN, MU = Term ;

linref
  A, A2, AP, AdA, AdN, AdV, Adv, Ant, CAdv, CN, Card, Cl, ClSlash, Comp, Conj, Det, Digits, Decimal,
  IAdv, IComp, IDet, IP, IQuant, Imp, Interj, N, N2, N3, NP, Num, Numeral, 
  Ord, PConj, GN, SN, PN, Phr, Pol, Predet, Prep, Pron, QCl, QS, Quant, RCl, RP, RS, S, SC, SSlash, 
  Subj, Temp, Tense, Text, Utt, V, V2, V2A, V2Q, V2S, V2V, V3, 
  VA, VP, VPSlash, VQ, VS, VV, Voc, ACard, DAP, LN, MU = \t -> t.f ++ t.x ++ t.swap ;

oper Term = {f,x,swap : Str ; par : Str * Str ; flat : Bool} ;

oper mkTerm = overload {
  mkTerm : Str -> Term = \f -> 
    {f = f ; x,swap = [] ; par = noPar ; flat = False} ;
  mkTerm : Str -> Term -> Term = \f,x -> 
    appTerm f (fullTerm x) ;
  mkTerm : Str -> (_,_ : Term) -> Term = \f,x,y -> 
    appTerm f (concatTerm x y) ; 
  mkTerm : Str -> (_,_,_ : Term) -> Term = \f,x,y,z -> 
    appTerm f (fullTerm x ++ concatTerm y z) ; 
  mkTerm : Str -> (_,_,_,_ : Term) -> Term = \f,x,y,z,u -> 
    appTerm f (fullTerm x ++ fullTerm y ++ fullTerm z ++ fullTerm u) ;
  mkTerm : {s:Str} -> Term = \s -> 
    {f = "\""++s.s++"\"" ; x,swap = [] ; par = noPar ; flat = False} ;
  } ;

  noPar  = <[],[]> ;
  yesPar = <"("++SOFT_BIND,SOFT_BIND++")"> ;

  appTerm : Str -> Str -> Term = \f,x -> {f = f ; x = x ; swap = [] ; par = yesPar ; flat = False} ;
  useTerm : Term -> Str = \t -> t.f ++ t.x ++ t.swap ;
  fullTerm : Term -> Str = \t -> t.par.p1 ++ t.f ++ t.x ++ t.swap ++ t.par.p2 ;
  flatTerm : Term -> Term = \t -> {f = [] ; x = t.x ; swap = t.swap ; par = noPar ; flat = False} ;
  flatIfTerm : Term -> Term = \t -> case t.flat of {
    True  => flatTerm t ;
    False => t
    } ;
  mkFlat : Term -> Term = \t -> {f = t.f ; x = t.x ; swap = t.swap ; par = t.par ; flat = True} ;
  mkSwap : Term -> Term = \t -> {f = t.f ; x = [] ; swap = t.x ; par = t.par ; flat = False} ;
  mkSwapTerm : Str -> Term -> Term -> Term = \f,x,y -> 
    let xs = fullTerm x ; ys = fullTerm y in
    {f = f ; x = xs ; swap = ys ; par = yesPar ; flat = False} ;
  concatTerm : Term -> Term -> Str = \t,u ->
    (t.par.p1 ++ t.f ++ t.x ++ t.par.p2 ++ fullTerm u ++ t.swap) ;

  hide : Str -> Str = \f -> [] ;
  hideOpt : Str -> Str = \f -> [] {-| f-} ;

}
