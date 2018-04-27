abstract Parse = 
  Noun,
  Verb, 
  Adjective,
  Adverb,
  Numeral,
  Sentence, 
  Question,
  Relative,
  Conjunction,
  Phrase,
  Text,
  Idiom,
  Tense,
  WordNet,
  Documentation ** {
  
  flags
    startcat = Phr ;

fun
  CompoundN : N -> N -> N ;
  GerundNP : VP -> NP ;
  PresPartAP : VP -> AP ;
  InOrderToVP : VP -> Adv ;
  has_age_VP : Num -> VP ;

-- INJECT

}

