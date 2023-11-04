concrete ParsePol of Parse =
  NounPol - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbPol - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectivePol - [ReflA2, CAdvAP, AdjOrd],
  AdverbPol - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentencePol - [EmbedVP],
  QuestionPol,
  RelativePol,
  ConjunctionPol,
  PhrasePol - [UttAP, UttVP],
  IdiomPol,
  TenseX - [CAdv,IAdv,AdV,SC],
  NamesPol,
  ParseExtendPol,
  WordNetPol,
  DocumentationPol
  ** {

flags
  case_sensitive = off;

} ;
