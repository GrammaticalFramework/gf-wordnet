concrete ParseNor of Parse =
  NounNor - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbNor - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP, UseCopula],
  AdjectiveNor - [ReflA2, CAdvAP, AdjOrd],
  AdverbNor - [ComparAdvAdj, ComparAdvAdjS, AdnCAdv],
  SentenceNor - [EmbedVP],
  QuestionNor,
  RelativeNor,
  ConjunctionNor,
  PhraseNor - [UttAP, UttVP],
  IdiomNor,
  TenseNor,
  NamesNor,
  ParseExtendNor,
  WordNetNor,
  ConstructionNor - [Language, InLanguage, languageNP, languageCN,
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
                     turkish_Language, urdu_Language, weather_adjCl],
  DocumentationNor ** {

flags
  case_sensitive = off;

} ;
