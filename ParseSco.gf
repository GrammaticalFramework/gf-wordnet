--# -path=.:../abstract:../common:../api
concrete ParseSco of Parse =
  NounSco - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbSco - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveSco - [ReflA2,CAdvAP,AdjOrd],
  AdverbSco - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceSco - [UseCl, EmbedVP],
  QuestionSco,
  RelativeSco - [IdRP],
  ConjunctionSco,
  PhraseSco - [UttAP, UttVP],
  IdiomSco,
  TenseX - [Pol,PPos,PNeg,SC,CAdv],
  NamesSco,
  ParseExtendSco,
  WordNetSco,
  ConstructionSco - [Language, InLanguage, languageNP, languageCN,
                     afrikaans_Language, amharic_Language, arabic_Language,
                     bulgarian_Language, catalan_Language, chinese_Language,
                     danish_Language, dutch_Language, english_Language,
                     estonian_Language, finnish_Language, french_Language,
                     german_Language, greek_Language, hebrew_Language,
                     hindi_Language, japanese_Language, italian_Language,
                     latin_Language, latvian_Language, maltese_Language,
                     nepali_Language, norwegian_Language, persian_Language,
                     polish_Language, punjabi_Language, romanian_Language,
                     russian_Language, sindhi_Language, spanish_Language,
                     swahili_Language, swedish_Language, thai_Language,
                     turkish_Language, urdu_Language],
  DocumentationSco ** open ResSco, ExtraEng, Prelude in {

flags
  case_sensitive = off;

lin
  PPos = {s = [] ; p = CPos} ;
  PNeg = {s = [] ; p = CNeg (variants {True; False})} ; -- contracted: don't

  UseCl = variants {SentenceEng.UseCl; ExtraEng.ContractedUseCl} ;
  
  IdRP = which_who_RP ;

-- INJECT

} ;
