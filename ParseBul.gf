--# -path=.:../abstract:../common
concrete ParseBul of Parse =
  NounBul,
  VerbBul,
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
  TenseX - [CAdv,IAdv],
  ExtendBul [N, CompoundN],
  WordNetBul,
  DocumentationBul
  ** open MorphoBul, ResBul, (S = StructuralBul), (L = LexiconBul), (E = ExtraBul), ParadigmsBul, Prelude in {

-- INJECT

} ;
