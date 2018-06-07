--# -path=.:../abstract:../common
concrete ParseBul of Parse =
  NounBul - [PPartNP],
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
  TenseX - [CAdv,IAdv,SC],
  ParseExtendBul,
  ConstructionBul,
  WordNetBul,
  DocumentationBul
  ** open MorphoBul, ResBul, (S = StructuralBul), (L = LexiconBul), (E = ExtendBul), ParadigmsBul, Prelude in {

-- INJECT

} ;
