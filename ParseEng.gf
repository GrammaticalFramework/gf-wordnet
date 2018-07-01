--# -path=.:../abstract:../common:../api
concrete ParseEng of Parse =
  NounEng - [PPartNP, UseN2, RelNP, DetNP],
  VerbEng - [PassV2],
  AdjectiveEng,
  AdverbEng,
  NumeralEng,
  SentenceEng - [UseCl],
  QuestionEng,
  RelativeEng - [IdRP],
  ConjunctionEng,
  PhraseEng - [UttAP],
  TextX - [Pol,PPos,PNeg,SC],
  IdiomEng,
  TenseX - [Pol,PPos,PNeg,SC],
  ParseExtendEng,
  WordNetEng,
  ConstructionEng,
  DocumentationEng
  ** open MorphoEng, ResEng, ParadigmsEng, IrregEng, (E = ExtendEng), (G = GrammarEng), SentenceEng, ExtraEng, Prelude in {

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg (variants {True; False})} ; -- contracted: don't

  UseCl = variants {SentenceEng.UseCl; ExtraEng.ContractedUseCl} ;
  
  IdRP = which_who_RP ;

-- INJECT

} ;
