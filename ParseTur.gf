--# -path=.:../abstract:../common:../api
concrete ParseTur of Parse =
  NounTur - [PPartNP, UseN2, RelNP, DetNP], --*
  VerbTur - [PassV2, ReflVP, ComplVV, SlashV2V, SlashVV, SlashV2VNP], --*
  AdjectiveTur - [ReflA2,CAdvAP],
  AdverbTur - [ComparAdvAdj,ComparAdvAdjS,AdnCAdv],
  SentenceTur - [EmbedVP],
  QuestionTur,
  RelativeTur,
  ConjunctionTur,
  PhraseTur - [UttAP,UttVP],
  TextX - [Temp,Pol,SC,Tense,Adv,CAdv,AdN,Ant], -- PPos,PNeg,CAdv
  IdiomTur,
  TenseX - [Temp,Pol,SC,Tense,Adv,CAdv,AdN,Ant], -- PPos,PNeg,CAdv
  WordNetTur
  ** {

-- INJECT

} ;

