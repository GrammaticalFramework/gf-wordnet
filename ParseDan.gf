concrete ParseDan of Parse =
  NounDan - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbDan - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveDan - [ReflA2, CAdvAP, AdjOrd],
  AdverbDan - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceDan - [EmbedVP],
  QuestionDan,
  RelativeDan,
  ConjunctionDan,
  PhraseDan - [UttAP, UttVP],
  IdiomDan,
  TenseDan,
  NamesDan,
  WordNetDan,
  DocumentationDan ** {

flags
  case_sensitive = off;

} ;
