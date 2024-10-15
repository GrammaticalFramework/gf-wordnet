concrete ParseNor of Parse =
  NounNor - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbNor - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveNor - [ReflA2, CAdvAP, AdjOrd],
  AdverbNor - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceNor - [EmbedVP],
  QuestionNor,
  RelativeNor,
  ConjunctionNor,
  PhraseNor - [UttAP, UttVP],
  IdiomNor,
  TenseNor,
  NamesNor,
  WordNetNor,
  DocumentationNor ** {

flags
  case_sensitive = off;

} ;
