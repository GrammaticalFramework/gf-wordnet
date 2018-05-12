--# -path=.:../scandinavian:../abstract:../common:../api
concrete ParseSwe of Parse =
  NounSwe,
  VerbSwe,
  AdjectiveSwe,
  AdverbSwe,
  NumeralSwe,
  SentenceSwe,
  QuestionSwe,
  RelativeSwe,
  ConjunctionSwe,
  PhraseSwe,
  TextX -[Tense,Temp],
  IdiomSwe,
  TenseSwe,
  ExtendSwe [N, CompoundN],
  WordNetSwe,
  DocumentationSwe
  ** open ParadigmsSwe, (I = IrregSwe), (C = CommonScand), (R = ResSwe), (MorphoSwe = MorphoSwe), (L = LexiconSwe), (M = MakeStructuralSwe), (S = SyntaxSwe), Prelude in {

-- INJECT

} ;
