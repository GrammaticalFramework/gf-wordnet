concrete ParseTel of Parse =
  NounTel - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbTel - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP],
  AdjectiveTel - [ReflA2, CAdvAP, AdjOrd],
  AdverbTel - [AdnCAdv, ComparAdvAdj, ComparAdvAdjS],
  SentenceTel - [EmbedVP],
  QuestionTel,
  RelativeTel,
  ConjunctionTel,
  PhraseTel - [UttAP, UttVP],
  IdiomTel,
  NamesTel,
  TenseX - [Adv, AdN, SC],
  ParseExtendTel,
  WordNetTel,
  DocumentationTel ** {
}
