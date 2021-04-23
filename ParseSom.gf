concrete ParseSom of Parse =
  NounSom - [PPartNP, UseN2, RelNP, DetNP],
  VerbSom - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveSom - [ReflA2, CAdvAP],
  AdverbSom - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceSom - [EmbedVP],
  QuestionSom,
  RelativeSom,
  PhraseSom - [UttAP, UttVP],
  IdiomSom,
  TenseX - [CAdv,IAdv,AdV,Adv,SC],
  WordNetSom
  ** {

flags
  case_sensitive = off;

} ;
