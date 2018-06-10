--# -path=.:../abstract:../common
concrete ParseBul of Parse =
  NounBul - [PPartNP, UseN2],
  VerbBul - [PassV2],
  AdjectiveBul,
  AdverbBul,
  NumeralBul,
  SentenceBul,
  QuestionBul,
  RelativeBul,
  ConjunctionBul,
  PhraseBul,
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
