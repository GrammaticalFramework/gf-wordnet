concrete ParseMkd of Parse = 
  NounMkd - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbMkd - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveMkd - [ReflA2],
  AdverbMkd - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceMkd - [EmbedVP],
  PhraseMkd - [UttAP,UttVP],
  ParseExtendMkd,
  TenseX,
  WordNetMkd,
  DocumentationMkd ** {

flags
  case_sensitive = off;

}
