concrete ParseExtendFin of ParseExtend = 
  ExtendFin - [iFem_Pron, youPolFem_Pron, weFem_Pron, youPlFem_Pron, theyFem_Pron, GenNP, DetNPMasc, DetNPFem, FocusAP,
               CompVP, InOrderToVP, PurposeVP, ComplGenVV, ReflRNP, ProDrop, UncontractedNeg, AdvIsNPAP, ExistCN, NominalizeVPSlashNP], NumeralFin - [num], PunctuationX **
 open MorphoFin, ResFin, ParadigmsFin, StemFin, (G=GrammarFin), (S=SyntaxFin), Prelude in {

lin PhrUttMark pconj utt voc mark = {s = pconj.s ++ utt.s ++ voc.s ++ SOFT_BIND ++ mark.s} ;

    ComplVV v ant pol vp = 
      insertObj 
        (\\_,b,a => infVPGen pol.p v.sc b a vp (vvtype2infform v.vi)) 
        (predSV {s = v.s ; 
                sc = case vp.s.sc of {
                  SCNom => v.sc ;        -- minun täytyy pestä auto
                  c     => c             -- minulla täytyy olla auto
                  } ;
                h = v.h ; p = v.p
               }
         ) ;

	UttAP  p ap  = {s = ap.s ! False ! NCase (complNumAgr p.a) Nom} ;
	UttVPS p vps = {s = vps.s ! p.a} ;

    SlashV2V v ant p vp = 
      insertObj (\\_,b,a => infVP v.sc b a vp (vvtype2infform v.vi)) (predSV v) ** {c2 = v.c2} ; ----

    UseDAP, UseDAPFem, UseDAPMasc = \dap -> 
      let
        n : ParadigmsFin.Number = case dap.isNum of {
          True => Sg ;
          _ => dap.n
          } ;
      in {
        s = \\c => let k = npform2case n c in
                 dap.sp ! k ; -- det.s2 is possessive suffix 
        a = agrP3 (case dap.isDef of {
            False => Sg ;  -- autoja menee; kolme autoa menee
            _ => dap.n
            }) ;
        isPron = False ; isNeg = dap.isNeg
      } ;

lincat 
  Sub1000000000 = {s : CardOrd => Str ; n : MorphoFin.Number} ;

lin 
  num x = x ;
  pot3as4 n = n ;

lin whatSgFem_IP, whatSgNeut_IP = G.whatSg_IP ;

lin that_RP = G.IdRP ;

-- AR 2019-09-12
lin gen_Quant = S.a_Quant ;
lin IdRP = S.that_RP ;
----lin ReflVPSlash vpslash rnp = 

}
