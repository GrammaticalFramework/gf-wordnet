--# -path=.:../scandinavian:../abstract:../common:../api
concrete ParseSwe of Parse =
  NounSwe - [PPartNP, UseN2, RelNP, DetNP],
  VerbSwe - [PassV2],
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
  ParseExtendSwe,
  WordNetSwe,
  ConstructionSwe,
  DocumentationSwe
  ** open ParadigmsSwe, (I = IrregSwe), (C = CommonScand), (R = ResSwe), (MorphoSwe = MorphoSwe), (L = LexiconSwe), (M = MakeStructuralSwe), (E = ExtendSwe), (G = GrammarSwe), Prelude in {

-- INJECT

} ;
