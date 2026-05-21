--# -path=.:../gf-rgl/src/hungarian:../gf-rgl/src/abstract:../gf-rgl/src/common:../gf-rgl/src/api:../gf-rgl/src/prelude
concrete ParseHun of Parse =
	  NounHun - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbHun - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveHun - [ReflA2,CAdvAP,AdjOrd],
  AdverbHun - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceHun - [EmbedVP],
  QuestionHun,
  RelativeHun,
  ConjunctionHun,
  PhraseHun - [UttAP, UttVP],
  IdiomHun,
  TenseX - [Adv, AdN, SC],
  NamesHun,
  ParseExtendHun,
  ConstructionHun - [Language, InLanguage, languageNP, languageCN,
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
  WordNetHun ** {

flags
  case_sensitive = off;

} ;
