concrete ParseKor of Parse =
  NounKor - [PPartNP, UseN2, RelNP, DetNP],
  VerbKor - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveKor - [ReflA2, CAdvAP],
  AdverbKor - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceKor - [EmbedVP],
  QuestionKor,
  RelativeKor,
  PhraseKor - [UttAP, UttVP],
  IdiomKor,
  TenseX - [CAdv,IAdv,AdV,SC],
  ParseExtendKor,
  WordNetKor,
  ConstructionKor
  ** {

flags
  case_sensitive = off;

} ;
