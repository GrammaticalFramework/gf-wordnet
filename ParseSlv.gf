--# -path=.:../abstract:../common:../api
concrete ParseSlv of Parse =
  NounSlv - [PPartNP, UseN2, RelNP, DetNP], --*
  VerbSlv - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP], --*
  AdjectiveSlv - [ReflA2,CAdvAP],
  AdverbSlv - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceSlv - [EmbedVP],
  QuestionSlv,
  ConjunctionSlv,
  PhraseSlv - [UttAP,UttVP],
  IdiomSlv,
  TenseX,
  WordNetSlv,
  DocumentationSlv
  ** {

-- INJECT

} ;

