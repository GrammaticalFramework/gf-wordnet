--# -path=.:../abstract:../common
concrete ParseBul of Parse =
  NounBul - [PPartNP, UseN2, RelNP, DetNP],
  VerbBul - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP],
  AdjectiveBul - [ReflA2],
  AdverbBul,
  NumeralBul,
  SentenceBul - [EmbedVP],
  QuestionBul,
  RelativeBul,
  ConjunctionBul,
  PhraseBul - [UttAP, UttVP],
  TextBul,
  IdiomBul,
  TenseX - [CAdv,IAdv,AdV,SC],
  ParseExtendBul,
  ConstructionBul,
  WordNetBul,
  DocumentationBul
  ** open MorphoBul, ResBul, (G = GrammarBul), (L = LexiconBul), (E = ExtendBul), ParadigmsBul, Prelude in {

-- INJECT

} ;
