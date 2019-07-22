--# -path=.:../scandinavian:../abstract:../common:../api
concrete ParseSwe of Parse =
  NounSwe - [PPartNP, UseN2, RelNP, DetNP],
  VerbSwe - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP],
  AdjectiveSwe - [ReflA2, CAdvAP],
  AdverbSwe - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceSwe - [EmbedVP],
  QuestionSwe,
  RelativeSwe,
  ConjunctionSwe,
  PhraseSwe - [UttAP, UttVP],
  IdiomSwe,
  TenseSwe,
  ParseExtendSwe,
  WordNetSwe,
  ConstructionSwe,
  DocumentationSwe
  ** open ParadigmsSwe, (I = IrregSwe), (C = CommonScand), (R = ResSwe), (MorphoSwe = MorphoSwe), (L = LexiconSwe), (M = MakeStructuralSwe), (E = ExtendSwe), (G = GrammarSwe), Prelude in {

-- INJECT

} ;
