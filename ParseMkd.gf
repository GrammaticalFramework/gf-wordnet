concrete ParseMkd of Parse = 
  NounMkd - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbMkd - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveMkd - [ReflA2, CAdvAP, AdjOrd],
  AdverbMkd - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceMkd - [EmbedVP],
  QuestionMkd,
  RelativeMkd,
  ConjunctionMkd,
  PhraseMkd - [UttAP,UttVP],
  IdiomMkd,
  ParseExtendMkd,
  TenseMkd,
  WordNetMkd,
  DocumentationMkd ** {

flags
  case_sensitive = off;

}
