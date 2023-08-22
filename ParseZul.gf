--# -path=.:../abstract:../common:../api
concrete ParseZul of Parse =
  NounZul - [PPartNP, UseN2, RelNP, DetNP, NumDigits],
  VerbZul - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP, UseCopula],
  AdjectiveZul - [ReflA2,CAdvAP],
  AdverbZul - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceZul - [UseCl, EmbedVP],
  QuestionZul,
  RelativeZul - [IdRP],
  -- ConjunctionZul,
  PhraseZul - [UttAP, UttVP],
  IdiomZul,
  TenseX - [Pol,PPos,PNeg,SC,Adv,IAdv,Temp,TTAnt,CAdv],
  -- NamesZul,
  ParseExtendZul,
  WordNetZul {- ,
  ConstructionZul - [Language, InLanguage, languageNP, languageCN,
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
  DocumentationZul -} ** open ResZul, Prelude in {

flags
  case_sensitive = off;

} ;
