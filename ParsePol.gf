concrete ParsePol of Parse =
  NounPol - [PPartNP, UseN2, RelNP, DetNP],
  VerbPol - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectivePol - [ReflA2, CAdvAP],
  AdverbPol - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentencePol - [EmbedVP],
  QuestionPol,
  RelativePol,
  PhrasePol - [UttAP, UttVP],
  IdiomPol,
  TenseX - [CAdv,IAdv,AdV,SC],
  ParseExtendPol,
  WordNetPol
  ** {

flags
  case_sensitive = off;

} ;
