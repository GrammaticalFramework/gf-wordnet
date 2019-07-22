--# -path=.:../abstract:../common:../api
concrete ParseTur of Parse =
  NounTur - [PPartNP, UseN2, RelNP, DetNP], --*
  VerbTur - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP], --*
  AdjectiveTur - [ReflA2,CAdvAP],
  AdverbTur - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceTur - [EmbedVP],
  QuestionTur,
  RelativeTur,
  ConjunctionTur,
  PhraseTur - [UttAP,UttVP],
  IdiomTur,
  TenseX - [CAdv,AdN],
  WordNetTur
  ** {

-- INJECT

} ;

