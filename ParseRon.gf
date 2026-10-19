--# -path=.:../abstract:../common:../api
concrete ParseRon of Parse =
  NounRon - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbRon - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveRon - [ReflA2,CAdvAP,AdjOrd],
  AdverbRon - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceRon - [EmbedVP],
  QuestionRon,
  RelativeRon,
  ConjunctionRon,
  PhraseRon - [UttAP, UttVP],
  IdiomRon,
  TenseX - [CAdv,SC,Temp,TTAnt,Tense,TPres,TPast,TFut,TCond,Pol,PPos,PNeg],
  TenseRon,
  NamesRon,
  ParseExtendRon,
  ConstructionRon - [Language, InLanguage, languageNP, languageCN,
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
  WordNetRon,
  DocumentationRon
  ** {

flags
  case_sensitive = off;

} ;
