concrete ParseNno of Parse =
  NounNno - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbNno - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveNno - [ReflA2, CAdvAP, AdjOrd],
  AdverbNno - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceNno - [EmbedVP],
  QuestionNno,
  RelativeNno,
  ConjunctionNno,
  PhraseNno - [UttAP, UttVP],
  IdiomNno,
  TenseNno,
  NamesNno,
  WordNetNno,
  DocumentationNno ** {

flags
  case_sensitive = off;

} ;
