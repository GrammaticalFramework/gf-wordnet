concrete ParseAra of Parse =
  NounAra - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbAra - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveAra - [ReflA2, CAdvAP, AdjOrd],
  AdverbAra - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceAra - [EmbedVP],
  QuestionAra,
  RelativeAra,
  ConjunctionAra,
  PhraseAra - [UttAP, UttVP],
  IdiomAra,
  TenseX - [Utt, CAdv,IAdv,AdV,SC,Adv],
  NamesAra,
  ParseExtendAra,
  WordNetAra,
  DocumentationAra
  ** {

flags
  case_sensitive = off;

} ;
