abstract Parse = 
  Noun - [PPartNP, UseN2, RelNP, DetNP],
  Verb - [PassV2, ReflVP, ComplVV, SlashVV, SlashV2V, SlashV2VNP], 
  Adjective - [ReflA2],
  Adverb,
  Numeral,
  Sentence - [EmbedVP], 
  Question,
  Relative,
  Conjunction,
  Phrase - [UttAP, UttVP],
  Text,
  Idiom,
  Tense,
  ParseExtend,
  Construction,
  WordNet,
  Documentation ** {
  
  flags
    startcat = Phr ;

-- INJECT

}

