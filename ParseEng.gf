--# -path=.:../abstract:../common:../api
concrete ParseEng of Parse =
  NounEng - [PPartNP, UseN2, RelNP, DetNP],
  VerbEng - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP],
  AdjectiveEng - [ReflA2,CAdvAP],
  AdverbEng - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceEng - [UseCl, EmbedVP],
  QuestionEng,
  RelativeEng - [IdRP],
  ConjunctionEng,
  PhraseEng - [UttAP, UttVP],
  TextX - [Pol,PPos,PNeg,SC,CAdv],
  IdiomEng,
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  ParseExtendEng,
  WordNetEng,
  ConstructionEng,
  DocumentationEng
  ** open MorphoEng, ResEng, ParadigmsEng, IrregEng, (E = ExtendEng), (G = GrammarEng), (C = ConstructX), SentenceEng, ExtraEng, Prelude in {

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg (variants {True; False})} ; -- contracted: don't

  UseCl = variants {SentenceEng.UseCl; ExtraEng.ContractedUseCl} ;
  
  IdRP = which_who_RP ;

-- INJECT

} ;
