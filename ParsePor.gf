--# -path=.:../abstract:../common:../api
concrete ParsePor of Parse =
  CommonX - [Temp,Pol,SC,Tense],
  NounPor - [PPartNP, UseN2, RelNP, DetNP],
  VerbPor - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP],
  AdjectivePor - [ReflA2,CAdvAP],
  AdverbPor - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentencePor - [UseCl, EmbedVP],
  QuestionPor,
  RelativePor - [IdRP],
  ConjunctionPor,
  PhrasePor - [UttAP, UttVP],
--  TextX - [Pol,PPos,PNeg,SC,CAdv],
  IdiomPor,
--  TenseX - [Temp,Pol,PPos,PNeg,SC,CAdv],
  ParseExtendPor,
  WordNetPor,
  ConstructionPor,
  DocumentationPor
  ** open MorphoPor, ResPor, ParadigmsPor, IrregPor, (E = ExtendPor), (G = GrammarPor), (C = ConstructX), SentencePor, ExtraPor, Prelude in {

-- INJECT

} ;
