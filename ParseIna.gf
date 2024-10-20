--# -path=.:../abstract:../common:../api
concrete ParseIna of Parse =
  NounIna - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbIna - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveIna - [ReflA2,CAdvAP,AdjOrd],
  AdverbIna - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceIna - [EmbedVP],
  QuestionIna,
  RelativeIna,
  ConjunctionIna,
  PhraseIna - [UttAP, UttVP],
  IdiomIna,
  TenseX,
  WordNetIna,
  DocumentationIna
  ** {
}
