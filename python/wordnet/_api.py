import pgf
import Parse as w

grammar = w.__pgf__

def linearize(lang : str,e : pgf.Expr) -> str:
    """
    Given a language code (bul,eng,swe,..)
    produces the linearization
    """
    return grammar.languages["Parse"+lang.title()].linearize(e)

def langs():
    return [cnc_name[5:].lower() for cnc_name in grammar.languages if cnc_name != "ParseAPI"]

def __types__(args):
    types = []
    for arg in args:
        if isinstance(arg,pgf.ExprLit):
            if isinstance(arg.val, float):
                types.append("Float")
            elif isinstance(arg.val, int):
                types.append("Int")
            elif isinstance(arg.val, str):
                types.append("String")
        elif isinstance(arg,pgf.Expr):
            (fun,_) = arg.unpack()
            types.append(w.__pgf__.functionType(fun).cat)
        else:
            types.append(type(arg))
    return types

def __no_match__(name,args):
    return TypeError("no overload for function "+name+" with argument types "+" ".join(map(str,args)))

fullStopPunct = w.FullStop
questMarkPunct = w.QuestMark
exclMarkPunct = w.ExclMark

def mkPhr(*args):
  """
  Constructs a new phrase in one of the following ways:
    mkPhr(conj: PConj, utterance: Utt, vocative: Voc) -> Phr
                              # but sleep, my friend
    mkPhr(sentence: S) -> Phr # she won't sleep
    mkPhr(clause: Cl) -> Phr  # she sleeps
    mkPhr(question: QS) -> Phr
                              # would she sleep
    mkPhr(imperative: Imp) -> Phr
                              # sleep
  """
  match __types__(args):
    case ["PConj","Utt","Voc"]:
      return w.PhrUtt(args[0],args[1],args[2])
    case ["PConj","Utt","Voc","Mark"]:
      return w.PhrUttMark(args[0],args[1],args[2],args[3])
    case ["Utt","Voc"]:
      return w.PhrUtt(w.NoPConj,args[0],args[1])
    case ["Utt","Voc","Mark"]:
      return w.PhrUttMark(w.NoPConj,args[0],args[1],args[2])
    case ["PConj","Utt"]:
      return w.PhrUtt(args[0],args[1],w.NoVoc)
    case ["PConj","Utt","Mark"]:
      return w.PhrUttMark(args[0],args[1],w.NoVoc,args[2])
    case ["Utt"]:
      return w.PhrUtt(w.NoPConj,args[0],w.NoVoc)
    case ["Utt","Mark"]:
      return w.PhrUttMark(w.NoPConj,args[0],w.NoVoc,args[1])
    case ["S"]:
      return w.PhrUtt(w.NoPConj,w.UttS(args[0]),w.NoVoc)
    case ["Cl"]:
      return w.PhrUtt(w.NoPConj,w.UttS(w.UseCl(w.TTAnt(w.TPres,w.ASimul),w.PPos,args[0])),w.NoVoc)
    case ["QS"]:
      return w.PhrUtt(w.NoPConj,w.UttQS(args[0]),w.NoVoc)
    case ["Imp"]:
      return w.PhrUtt(w.NoPConj,w.UttImpSg(w.PPos,args[0]),w.NoVoc)
    case types:
      raise __no_match__("mkPhr",types)

def mkPConj(*args):
  """
  Constructs a phrase-beginning conjunction from a conjunction:
    mkPConj(conj: Conj) -> PConj
                              # and now
  You can also use:
    noPConj : PConj
  """
  match __types__(args):
    case ["Conj"]:
      return w.PConjConj(args[0])
    case types:
      raise __no_match__("mkPConj",types)

noPConj = w.NoPConj

def mkVoc(*args):
  """
  Constructs a vocative from a noun phrase:
    mkVoc(noun_phrase: NP) -> Voc
                              # yes, my friend
  You can also use:
    noVoc : Voc
  """
  match __types__(args):
    case ["NP"]:
      return w.VocNP(args[0])
    case types:
      raise __no_match__("mkVoc",types)

noVoc = w.NoVoc

def mkUtt(*args):
  """
  Constructs an utterance from sentence, question, etc:
    mkUtt(sentence: S) -> Utt
     	                      # she slept
    mkUtt(clause: Cl) -> Utt  # she sleeps
    mkUtt(question: QS) -> Utt
                              # who didn't sleep
    mkUtt(question_clause: QCl) -> Utt
                              # who sleeps
    mkUtt(polarity: Pol, imperative: Imp) -> Utt
                              # don't be men
    mkUtt(interogative_pron: IP) -> Utt
                              # who
    mkUtt(interogative_adv: IAdv) -> Utt
                              # why
    mkUtt(noun_phrase: NP) -> Utt
                              # this man
    mkUtt(adverb: Adv) -> Utt # here
    mkUtt(verb_phrase: VP) -> Utt
                              # to sleep
    mkUtt(common_noun: CN) -> Utt
                              # beer
    mkUtt(adjectival_phrase: AP) -> Utt
                              # good
    mkUtt(cardinal: Card) -> Utt
                              # five
  """
  match __types__(args):
    case ["S"]:
      return w.UttS(args[0])
    case ["Cl"]:
      return w.UttS(w.UseCl(w.TTAnt(w.TPres,w.ASimul),w.PPos,args[0]))
    case ["QS"]:
      return w.UttQS(args[0])
    case ["QCl"]:
      return w.UttQS(w.TUseQCl(w.TPres,w.ASimul,w.PPos,args[0]))
    case ["Pol","Imp"]:
      return w.UttImpSg(args[0],args[1])
    case ["Imp"]:
      return w.UttImpSg(w.PPos,args[0])
    case ["IP"]:
      return w.UttIP(args[0])
    case ["IAdv"]:
      return w.UttIAdv(args[0])
    case ["NP"]:
      return w.UttNP(args[0])
    case ["Adv"]:
      return w.UttAdv(args[0])
    case ["VP"]:
      return w.UttVP(args[0],args[1],args[2],args[3])
    case ["CN"]:
      return w.UttCN(args[0])
    case ["AP"]:
      return w.UttAP(w.it_Pron,args[0])
    case ["Pron","AP"]:
      return w.UttAP(args[0],args[1])
    case ["Card"]:
      return w.UttCard(args[0])
    case types:
      raise __no_match__("mkUtt",types)

def lets_Utt(*args):
  """
  lets_Utt(verb_phrase: VP) -> Utt
                              # let's sleep
  """
  match __types__(args):
    case ["VP"]:
      return w.ImpPl1(args[0])
    case types:
      raise __no_match__("lets_Utt",types)

positivePol = w.PPos
negativePol = w.PNeg

simultaneousAnt = w.ASimul
anteriorAnt = w.AAnter

presentTense = w.TPres
pastTense = w.TPast
pastSimpleTense = w.TPastSimple
futureTense = w.TFut
conditionalTense = w.TCond

def mkTemp(*args):
  """
  mkTemp(tense: Tense, anteriority: Ant) -> Temp
                              # e.g. past + anterior
  You can use:
    simultaneousAnt, anteriorAnt : Ant

    presentTense, pastTense
    futureTense, conditionalTense : Tense  
  """
  match __types__(args):
    case ["Tense","Ant"]:
      return w.TTAnt(args[0],args[1])
    case types:
      raise __no_match__("mkTemp",types)

def mkS(*args):
  """
  Constructs a declarative sentence:
    mkS(clause: Cl) -> S      # she sleeps
    mkS(tense: Tense, clause: Cl) -> S
    mkS(anteriority: Ant, clause: Cl) -> S
    mkS(polarity: Pol, clause: Cl) -> S
    mkS(tense: Tense, anteriority: Ant, clause: Cl) -> S
    mkS(tense: Tense, polarity: Pol, clause: Cl) -> S
    mkS(anteriority: Ant, polarity: Pol, clause: Cl) -> S
    mkS(tense: Tense, anteriority: Ant, polarity: Pol, clause: Cl) -> S
                              # she wouldn't have slept
    mkS(temp: Temp, polarity: Pol, clause: Cl) -> S
                              # she wouldn't have slept
    mkS(conjunction: Conj, sentence1: S, sentence2: S) -> S
                              # she sleeps and I run
    mkS(conjunction: Conj, list: list[S]) -> S
                              # she sleeps, I run and you walk
    mkS(adverb:	Adv, sentence: S) -> S
                              # today she sleeps
  """
  match __types__(args):
    case ["Cl"]:
      return w.UseCl(w.TTAnt(w.TPres,w.ASimul),w.PPos,args[0])
    case ["Tense","Cl"]:
      return w.UseCl(w.TTAnt(args[0],w.ASimul),w.PPos,args[1])
    case ["Ant","Cl"]:
      return w.UseCl(w.TTAnt(w.TPres,args[0]),w.PPos,args[1])
    case ["Pol","Cl"]:
      return w.UseCl(w.TTAnt(w.TPres,w.ASimul),args[0],args[1])
    case ["Tense","Ant","Cl"]:
      return w.UseCl(w.TTAnt(args[0],args[1]),w.PPos,args[2])
    case ["Tense","Pol","Cl"]:
      return w.UseCl(w.TTAnt(args[0],w.ASimul),args[1],args[2])
    case ["Ant","Pol","Cl"]:
      return w.UseCl(w.TTAnt(w.TPres,args[0]),args[1],args[2])
    case ["Tense","Ant","Pol","Cl"]:
      return w.UseCl(w.TTAnt(args[0],args[1]),args[2],args[3])
    case ["Temp","Pol","Cl"]:
      return w.UseCl(args[0],args[1],args[2])
    case ["Conj","S","S"]:
      return w.ConjS(args[0],w.BaseS(args[1],args[2]))
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjS(args[0],mkList(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["Adv","S"]:
      return w.AdvS(args[0],args[1])
    case types:
      raise __no_match__("mkS",types)

def mkCl(*args):
  """
  Constructs a declarative clause, with all tenses
    mkCl(subject: NP, verb: V) -> Cl
                              # she sleeps
    mkCl(subject: NP, verb: V2, object: NP) -> Cl
                              # she loves him
    mkCl(subject: NP, verb: V3, object1: NP, object2: NP) -> Cl
                              # she sends it to him
    mkCl(subject: NP, verb: VV, verb_phrase: VP) -> Cl
                              # she wants to sleep
    mkCl(subject: NP, verb: VS, sentence: S) -> Cl
                              # she says that I sleep
    mkCl(subject: NP, verb: VQ, question: QS) -> Cl
                              # she wonders who sleeps
    mkCl(subject: NP, verb: VA, adjective: A) -> Cl
                              # she becomes old
    mkCl(subject: NP, verb: VA, adjectival_phrase: AP) -> Cl
                              # she becomes very old
    mkCl(subject: NP, verb: V2A, object: NP, adjective: A) -> Cl
                              # she paints it red
    mkCl(subject: NP, verb: V2A, object: NP, adjectival_phrase: AP) -> Cl
                              # she paints it red
    mkCl(subject: NP, verb: V2S, object: NP, sentence: S) -> Cl
                              # she answers to him that we sleep
    mkCl(subject: NP, verb: V2Q, object: NP, question: QS) -> Cl
                              # she asks him who sleeps
    mkCl(subject: NP, verb: V2V, object: NP, verb_phrase: VP) -> Cl
                              # she begs him to sleep
    mkCl(subject: NP, verb: VPSlash, object: NP) -> Cl
                              # she begs him to sleep here
    mkCl(subject: NP, adjective: A) -> Cl
                              # she is old
    mkCl(subject: NP, adjective: A, object: NP) -> Cl
                              # she is older than he
    mkCl(subject: NP, adjective: A2, object: NP) -> Cl
                              # she is married to him
    mkCl(subject: NP, adjectival_phrase: AP) -> Cl
                              # she is very old
    mkCl(subject: NP, verb_phrase: NP) -> Cl
                              # she is the woman
    mkCl(subject: NP, object: N) -> Cl
                              # she is a woman
    mkCl(subject: NP, object: CN) -> Cl
                              # she is an old woman
    mkCl(subject: NP, adverb: Adv) -> Cl
                              # she is here
    mkCl(subject: NP, verb_phrase: VP) -> Cl
                              # she always sleeps
    mkCl(subject: N) -> Cl
                              # there is a house
    mkCl(subject: CN) -> Cl
                              # there is an old house
    mkCl(subject: NP) -> Cl
                              # there are many houses
    mkCl(subject: NP, relative_sentence: RS) -> Cl
                              # it is she that sleeps
    mkCl(subject: Adv, sentence: S) -> Cl
                              # it is here that she sleeps
    mkCl(subject: V) -> Cl
                              # it rains
    mkCl(subject: VP) -> Cl
                              # it is raining
    mkCl(subject: SC, verb_phrase: VP) -> Cl
                              # that she sleeps is good
  """
  match __types__(args):
    case ["NP","V"]:
      return w.PredVP(args[0],w.UseV(args[1]))
    case ["NP","V2","NP"]:
      return w.PredVP(args[0],w.ComplV2(args[1],args[2]))
    case ["NP","V3","NP","NP"]:
      return w.PredVP(args[0],w.ComplSlash(w.Slash3V3(args[1],args[3]),args[2]))
    case ["NP","VV","VP"]:
      return w.PredVP(args[0],w.ComplVV(args[1],w.ASimul,w.PPos,args[2]))
    case ["NP","VS","S"]:
      return w.PredVP(args[0],w.ComplVS(args[1],args[2]))
    case ["NP","VQ","QS"]:
      return w.PredVP(args[0],w.ComplVQ(args[1],args[2]))
    case ["NP","VA","A"]:
      return w.PredVP(args[0],w.ComplVA(args[1],w.PositA(args[2])))
    case ["NP","VA","AP"]:
      return w.PredVP(args[0],w.ComplVA(args[1],args[2]))
    case ["NP","V2A","NP","A"]:
      return w.PredVP(args[0],w.ComplV2A(args[1],args[2],w.PositA(args[3])))
    case ["NP","V2A","NP","AP"]:
      return w.PredVP(args[0],w.ComplV2A(args[1],args[2],args[3]))
    case ["NP","V2S","NP","S"]:
      return w.PredVP(args[0],w.ComplSlash(w.SlashV2S(args[1],args[3]),args[2]))
    case ["NP","V2Q","NP","QS"]:
      return w.PredVP(args[0],w.ComplSlash(w.SlashV2Q(args[1],args[3]),args[2]))
    case ["NP","V2V","NP","VP"]:
      return w.PredVP(args[0],w.ComplSlash(w.SlashV2V(args[1],args[3]),args[2]))
    case ["NP","VPSlash","NP"]:
      return w.PredVP(args[0],w.ComplSlash(args[1],args[2]))
    case ["NP","A"]:
      return w.PredVP(args[0],w.UseComp(w.CompAP(w.PositA(args[1]))))
    case ["NP","A","NP"]:
      return w.PredVP(args[0],w.UseComp(w.CompAP(w.ComparA(args[1],args[2]))))
    case ["NP","A2","NP"]:
      return w.PredVP(args[0],w.UseComp(w.CompAP(w.ComplA2(args[1],args[2]))))
    case ["NP","AP"]:
      return w.PredVP(args[0],w.UseComp(w.CompAP(args[1])))
    case ["NP","NP"]:
      return w.PredVP(args[0],w.UseComp(w.CompNP(args[1])))
    case ["NP","N"]:
      return w.PredVP(args[0],w.UseComp(w.CompCN(w.UseN(args[1]))))
    case ["NP","CN"]:
      return w.PredVP(args[0],w.UseComp(w.CompCN(args[1])))
    case ["NP","Adv"]:
      return w.PredVP(args[0],w.UseComp(w.CompAdv(args[1])))
    case ["NP","VP"]:
      return w.PredVP(args[0],args[1])
    case ["N"]:
      return w.ExistNP(w.DetCN(w.DetQuant(w.IndefArt, w.NumSg), w.UseN(args[0])))
    case ["CN"]:
      return w.ExistNP(w.DetCN(w.DetQuant(w.IndefArt, w.NumSg), args[0]))
    case ["NP"]:
      return w.ExistNP(args[0])
    case ["NP","RS"]:
      return w.CleftNP(args[0],args[1])
    case ["Adv","S"]:
      return w.CleftAdv(args[0],args[1])
    case ["V"]:
      return w.ImpersCl(w.UseV(args[0]))
    case ["VP"]:
      return w.ImpersCl(args[0])
    case ["SC","VP"]:
      return w.PredSCVP(args[0],args[1])
    case types:
      raise __no_match__("mkCl",types)

def genericCl(*args):
  """
  genericCl(verb_phrase: VP) -> Cl
                              # one sleeps
  """
  match __types__(args):
    case ["VP"]:
      return w.GenericCl(args[0])
    case types:
      raise __no_match__("genericCl",types)

def mkVP(*args):
  """
  Constructs a verb phrase:
    mkVP(verb: V) -> VP       # to sleep
    mkVP(verb: V2, object: NP) -> VP
                              # to love him
    mkVP(verb: V3, object1: NP, object2: NP) -> VP
                              # to send it to him
    mkVP(verb: VV, verb_phrase: VP) -> VP
                              # to want to sleep
    mkVP(verb: VS, sentence: S) -> VP
                              # to know that she sleeps
    mkVP(verb: VQ, question: QS) -> VP
                              # to wonder who sleeps
    mkVP(verb: VA, adjectival_phrase: AP) -> VP
                              # to become red
    mkVP(verb: V2A, object: NP, adjectival_phrase: AP) -> VP
                              # to paint it red
    mkVP(verb: V2S, object: NP, sentence: S) -> VP
                              # to answer to him that she sleeps
    mkVP(verb: V2Q, object: NP, question: QS) -> VP
                              # to ask him who sleeps
    mkVP(verb: V2V, object: NP, verb_phrase: VP) -> VP
                              # to beg him to sleep
    mkVP(adjective: A) -> VP
                              # to be old
    mkVP(adjective: A, noun_phrase: NP) -> VP
                              # to be older than he
    mkVP(adjective: A2, noun_phrase: NP) -> VP
                              # to be married to him
    mkVP(adjectival_phrase: AP) -> VP
                              # to be very old
    mkVP(noun: N) -> VP       # to be a woman
    mkVP(common_noun: CN) -> VP
                              # to be an old woman
    mkVP(noun_phrase: NP) -> VP
                              # to be the woman
    mkVP(adverb: Adv) -> VP   # to be here
    mkVP(verb_phrase: VP, adverb: Adv) -> VP
                              # to sleep here
    mkVP(adverb: AdV, verb_phrase: VP) -> VP
                              # to always sleep
    mkVP(verb: VPSlash, noun_phrase: NP) -> VP
                              # to paint it black
    mkVP(verb: VPSlash) -> VP # to paint itself black
    mkVP(complement: Comp) -> VP
                              # to be warm
  """
  match __types__(args):
    case ["V"]:
      return w.UseV(args[0])
    case ["V2","NP"]:
      return w.ComplSlash(w.SlashV2a(args[0]),args[1])
    case ["V3","NP","NP"]:
      return w.ComplSlash(w.Slash3V3(args[0],args[2]),args[1])
    case ["VV","VP"]:
      return w.ComplVV(args[0],w.ASimul,w.PPos,args[3])
    case ["VV","Ant","Pol","VP"]:
      return w.ComplVV(args[0],args[1],args[2],args[3])
    case ["VS","S"]:
      return w.ComplVS(args[0],args[1])
    case ["VQ","QS"]:
      return w.ComplVQ(args[0],args[1])
    case ["VA","AP"]:
      return w.ComplVA(args[0],args[1])
    case ["V2A","NP","AP"]:
      return w.ComplV2A
    case ["V2S","NP","S"]:
      return w.ComplSlash(w.SlashV2S(args[0],args[2]),args[1])
    case ["V2Q","NP","QS"]:
      return w.ComplSlash(w.SlashV2Q(args[0],args[2]),args[1])
    case ["V2V","NP","VP"]:
      return w.ComplSlash(w.SlashV2V(args[0],args[2]),args[1])
    case ["A"]:
      return w.UseComp(w.CompAP(w.PositA(args[0])))
    case ["A","NP"]:
      return w.UseComp(w.CompAP(w.ComparA(args[0],args[1])))
    case ["A2","NP"]:
      return w.UseComp(w.CompAP(w.ComplA2(args[0],args[1])))
    case ["AP"]:
      return w.UseComp(w.CompAP(args[0]))
    case ["N"]:
      return w.UseComp(w.CompCN(w.UseN(args[0])))
    case ["CN"]:
      return w.UseComp(w.CompCN(args[0]))
    case ["NP"]:
      return w.UseComp(w.CompNP(args[0]))
    case ["Adv"]:
      return w.UseComp(w.CompAdv(args[0]))
    case ["VP","Adv"]:
      return w.AdvVP(args[0],args[1])
    case ["AdV","VP"]:
      return w.AdVVP(args[0],args[1])
    case ["VPSlash","NP"]:
      return w.ComplSlash(args[0],args[1])
    case ["VPSlash"]:
      return w.ReflVPSlash(args[0],w.ReflPron)
    case ["Comp"]:
      return w.UseComp(args[0])
    case types:
      raise __no_match__("mkVP",types)

def reflexiveVP(*args):
  """
  reflexiveVP(verb: V2) -> VP # to love itself
  reflexiveVP(verb: VPSlash) -> VP
                              # paint itself black
  """
  match __types__(args):
    case ["V2"]:
      return w.ReflVPSlash(w.SlashV2a(args[0]),w.ReflPron)
    case ["VPSlash"]:
      return w.ReflVPSlash(args[0],w.ReflPron)
    case types:
      raise __no_match__("reflexiveVP",types)

def passiveVP(*args):
  """
  passiveVP(verb: V2) -> VP   # to be loved
  passiveVP(verb: V2, agent: NP) -> VP
                              # to be loved by her
  """
  match __types__(args):
    case ["V2"]:
      return w.PassVPSlash(w.SlashV2a(args[0]))
    case ["V2","NP"]:
      return w.PassAgentVPSlash(w.SlashV2a(args[0]),args[1])
    case ["VPSlash"]:
      return w.PassVPSlash(args[0])
    case ["VPSlash","NP"]:
      return w.PassAgentVPSlash(args[0],args[1])
    case types:
      raise __no_match__("passiveVP",types)

def progressiveVP(*args):
  """
  progressiveVP(verb_phrase: VP) -> VP
                              # to be sleeping
  """
  match __types__(args):
    case ["VP"]:
      return w.ProgrVP(args[0])
    case types:
      raise __no_match__("progressiveVP",types)

def mkComp(*args):
  """
  Constructs a complement of copula, such as AP:
    mkComp(adjectival_phrase: AP) -> Comp
                              # to be old
    mkComp(noun_phrase: NP) -> Comp
                              # to be this man
    mkComp(adverb: Adv) -> Comp
                              # to be here
  """
  match __types__(args):
    case ["AP"]:
      return w.CompAP(args[0])
    case ["NP"]:
      return w.CompNP(args[0])
    case ["Adv"]:
      return w.CompAdv(args[0])
    case types:
      raise __no_match__("mkComp",types)

def mkSC(*args):
  """
  Constructs an embedded sentence or question:
    mkSC(sentence: S) -> SC   # that she sleeps
    mkSC(question: QS) -> SC  # who sleeps
    mkSC(anteriorit: Ant,polarity: Pol,pron: Pron,verb_phrase: VP) -> SC
                              # to sleep
  """
  match __types__(args):
    case ["S"]:
      return w.EmbedS(args[0])
    case ["QS"]:
      return w.EmbedQS(args[0])
    case ["Ant","Pol","Pron","VP"]:
      return w.EmbedVP(args[0],args[1],args[2],args[3])
    case types:
      raise __no_match__("mkSC",types)

def mkImp(*args):
  """
  Constructs an imperative:
    mkImp(verb_phrase: VP) -> Imp
                              # come to my house
    mkImp(verb: V) -> Imp     # come
    mkImp(verb: V2, object: NP) -> Imp
                              # buy it
  """
  match __types__(args):
    case ["VP"]:
      return w.ImpVP(args[0])
    case ["V"]:
      return w.ImpVP(w.UseV(args[0]))
    case ["V2","NP"]:
      return w.ImpVP(w.ComplV2(args[0],args[1]))
    case types:
      raise __no_match__("mkImp",types)

def mkNP(*args):
  """
  Constructs a noun phrase (subject or object)
    mkNP(quantifier: Quant, noun: N) -> NP
                              # this man
    mkNP(quantifier: Quant, common_noun: CN) -> NP
                              # this old man
    mkNP(quantifier: Quant, numeral: Num, common_noun: CN) -> NP
                              # these five old men
    mkNP(quantifier: Quant, numeral: Num, ordinal: Ord, common_noun: CN) -> NP
                              # these five old men
    mkNP(quantifier: Quant, numeral: Num, noun: N) -> NP
                              # these five men
    mkNP(determiner: Det, common_noun: CN) -> NP
                              # the five old men
    mkNP(determiner: Det, noun: N) -> NP
                              # the five men
    mkNP(numeral: Num, common_noun: CN) -> NP
                              # five old men
    mkNP(numeral: Num, ordinal: Ord, common_noun: CN) -> NP
                              # five old men
    mkNP(numeral: Num, noun: N) -> NP
                              # five men
    mkNP(numeral: Numeral, common_noun: CN) -> NP
                              # five old men
    mkNP(numeral: Numeral, ordinal: Ord, common_noun: CN) -> NP
                              # five old men
    mkNP(numeral: Numeral, noun: N) -> NP
                              # five men
    mkNP(decimal: Decimal, common_noun: CN) -> NP
                              # five old men
    mkNP(decimal: Decimal, noun: N) -> NP
                              # five men
    mkNP(digits: Digits, common_noun: CN) -> NP
                              # 51 old men
    mkNP(digits: Digits, noun: N) -> NP
                              # 51 men
    mkNP(cardinal: Card, common_noun: CN) -> NP
                              # forty-five old men
    mkNP(cardinal: Card, noun: N) -> NP
                              # forty-five men
    mkNP(pronoun: Pron, common_noun: CN) -> NP
                              # my old man
    mkNP(pronoun: Pron, noun: N) -> NP
                              # my man
    mkNP(proper_name: PN) -> NP
    mkNP(location_name: LN) -> NP
                              # Paris
    mkNP(pronoun: Pron) -> NP # we
    mkNP(quantifier: Quant) -> NP
                              # this
    mkNP(quantifier: Quant -> Num) -> NP
                              # these five
    mkNP(determiner: Det) -> NP
                              # the five best
    mkNP(common_noun: CN) -> NP
                              # old beer
    mkNP(noun: N) -> NP       # beer
    mkNP(predeterminer: Predet, noun_phrase: NP) -> NP
                              # only this woman
    mkNP(noun_phrase: NP, verb: V2) -> NP
                              # the man seen
    mkNP(noun_phrase: NP, adverb: Adv) -> NP
                              # Paris today
    mkNP(noun_phrase: NP, relative_sentence: RS) -> NP
                              # John, that walks ...
    mkNP(conjunct: Conj, np1: NP, np2: NP) -> NP
                              # this woman or John
    mkNP(conjunct: Conj, list: list[NP]) -> NP
                              # this woman, John or I
    mkNP(d: Decimal, unit: MU) -> NP
                              # this woman, John or I
  """
  match __types__(args):
    case ["Quant","N"]:
      return w.DetCN(w.DetQuant(args[0],w.NumSg),w.UseN(args[1]))
    case ["Quant","CN"]:
      return w.DetCN(w.DetQuant(args[0],w.NumSg),args[1])
    case ["Quant","Num","CN"]:
      return w.DetCN(w.DetQuant(args[0],args[1]),args[2])
    case ["Quant","Num","Ord","CN"]:
      return w.DetCN(w.DetQuantOrd(args[0],args[1],args[2]),args[3])
    case ["Quant","Num","N"]:
      return w.DetCN(w.DetQuant(args[0],args[1]),w.UseN(args[2]))
    case ["Det","CN"]:
      return w.DetCN(args[0],args[1])
    case ["Det","N"]:
      return w.DetCN(args[0],w.UseN(args[1]))
    case ["Num","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,args[0]),args[1])
    case ["Num","Ord","CN"]:
      return w.DetCN(w.DetQuantOrd(w.IndefArt,args[0],args[1]),args[2])
    case ["Num","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,args[0]),w.UseN(args[1]))
    case ["Numeral","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0]))),args[1])
    case ["Numeral","Ord","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0]))),args[1])
    case ["Numeral","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0]))),w.UseN(args[1]))
    case ["Decimal","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumDecimal(args[0]))),args[1])
    case ["Decimal","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumDecimal(args[0]))),w.UseN(args[1]))
    case ["Digits","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumDecimal(w.PosDecimal(args[0])))),args[1])
    case ["Digits","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumDecimal(w.PosDecimal(args[0])))),w.UseN(args[1]))
    case ["Card","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(args[0])),args[1])
    case ["Card","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(args[0])),w.UseN(args[1]))
    case ["Pron","CN"]:
      return w.DetCN(w.DetQuant(w.PossPron(args[0]),w.NumSg),args[1])
    case ["Pron","N"]:
      return w.DetCN(w.DetQuant(w.PossPron(args[0]),w.NumSg),w.UseN(args[1]))
    case ["PN"]:
      return w.UsePN(args[0])
    case ["LN"]:
      return w.UseLN(args[0])
    case ["Pron"]:
      return w.UsePron(args[0])
    case ["Quant"]:
      return w.UseDAP(w.DetDAP(w.DetQuant(args[0],w.sgNum)))
    case ["Quant","Num"]:
      return w.UseDAP(w.DetDAP(w.DetQuant(args[0],args[1])))
    case ["Det"]:
      return w.UseDAP(w.DetDAP(args[0]))
    case ["CN"]:
      return w.MassNP(args[0])
    case ["N"]:
      return w.MassNP(w.UseN(args[0]))
    case ["Predet","NP"]:
      return w.PredetNP(args[0],args[1])
    case ["NP","V2"]:
      return w.PPartNP
    case ["NP","Adv"]:
      return w.AdvNP(args[0],args[1])
    case ["NP","RS"]:
      return w.RelNP(args[0],args[1])
    case ["Conj","NP","NP"]:
      return w.ConjNP(args[0],w.BaseNP(args[1],args[2]))
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjNP(args[0],mkList(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["Quant","CN"]:
      return w.DetCN(w.DetQuant(args[0],w.NumSg),args[1])
    case [float,"MU"]:
      return w.QuantityNP(mkDecimal(args[0]),args[1])
    case types:
      raise __no_match__("mkNP",types)

i_NP = w.UsePron(w.i_Pron)

you_NP = w.UsePron(w.youSg_Pron)

youPol_NP = w.UsePron(w.youPol_Pron)

he_NP = w.UsePron(w.he_Pron)

she_NP = w.UsePron(w.she_Pron)

it_NP = w.UsePron(w.it_Pron)

we_NP = w.UsePron(w.we_Pron)

youPl_NP = w.UsePron(w.youPl_Pron)

they_NP = w.UsePron(w.they_Pron)

this_NP = w.UseDAP(w.DetDAP(w.DetQuant(w.this_Quant,w.NumSg)))

that_NP = w.UseDAP(w.DetDAP(w.DetQuant(w.that_Quant,w.NumSg)))

these_NP = w.UseDAP(w.DetDAP(w.DetQuant(w.this_Quant,w.NumPl)))

those_NP = w.UseDAP(w.DetDAP(w.DetQuant(w.that_Quant,w.NumPl)))

def mkDet(*args):
  """
  Constructs a determiner phrase
    mkDet(quant: Quant) -> Det
                              # this
    mkDet(quant: Quant, card: Card) -> Det
                              # these five
    mkDet(quant: Quant, ordinal: Ord) -> Det
                              # the fifth
    mkDet(quant: Quant, number: Num, ordinal: Ord) -> Det
                              # the five best
    mkDet(quant: Quant, number: Num) -> Det
                              # these
    mkDet(card: Card) -> Det  # five
    mkDet(digits: Digits) -> Det
                              # 51
    mkDet(decimal: Decimal) -> Det
                              # 51
    mkDet(numeral: Numeral) -> Det
                              # five
    mkDet(pronoun: Pron) -> Det
                              # my
    mkDet(pronoun: Pron, number: Num) -> Det
                              # my five
  You can also use:
    theSg_Det, thePl_Det, the_Det,
    aSg_Det, aPl_Det, a_Det,
    this_Det, that_Det, these_Det, those_Det : Det
  """
  match __types__(args):
    case ["Quant"]:
      return w.DetQuant(args[0],w.NumSg)
    case ["Quant","Card"]:
      return w.DetQuant(args[0],w.NumCard(args[1]))
    case ["Quant","Ord"]:
      return w.DetQuantOrd(args[0],w.NumSg,args[1])
    case ["Quant","Num","Ord"]:
      return w.DetQuantOrd(args[0],args[1],args[2])
    case ["Quant","Num"]:
      return w.DetQuant(args[0],args[1])
    case ["Card"]:
      return w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0])))
    case ["Digits"]:
      return w.DetQuant(w.IndefArt,w.NumCard(w.NumDecimal(w.PosDecimal(args[0]))))
    case ["Decimal"]:
      return w.DetQuant(w.IndefArt,w.NumCard(w.NumDecimal(args[0])))
    case ["Numeral"]:
      return w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0])))
    case ["Pron"]:
      return w.DetQuant(w.PossPron(args[0]),w.NumSg)
    case ["Pron","Num"]:
      return w.DetQuant(w.PossPron(args[0]),args[1])
    case types:
      raise __no_match__("mkDet",types)

theSg_Det = w.DetQuant(w.DefArt,w.NumSg)

thePl_Det = w.DetQuant(w.DefArt,w.NumPl)

the_Det = theSg_Det

aSg_Det = w.DetQuant(w.IndefArt,w.NumSg)

aPl_Det = w.DetQuant(w.IndefArt,w.NumPl)

a_Det = aSg_Det

this_Det = w.DetQuant(w.this_Quant,w.NumSg)

that_Det = w.DetQuant(w.that_Quant,w.NumSg)

these_Det = w.DetQuant(w.this_Quant,w.NumPl)

those_Det = w.DetQuant(w.that_Quant,w.NumPl)

def mkQuant(*args):
  """
  mkQuant(pronoun: Pron) -> Quant
  You can also use:
    the_Quant, a_Quant : Quant
  """
  match __types__(args):
    case ["Pron"]:
      return w.PossPron(args[0])
    case types:
      raise __no_match__("mkQuant",types)

the_Quant = w.DefArt

a_Quant = w.IndefArt

def mkNum(*args):
  """
  Constructs a number determining element
    mkNum(n: float) -> Num    # 3.14
    mkNum(n: int) -> Num      # 21
    mkNum(n: Numeral) -> Num  # thirty-five
    mkNum(digits: Digits) -> Num
                              # 21
    mkNum(decimal: Decimal) -> Num
                              # 21
    mkNum(digit: Digit) -> Num
                              # five
    mkNum(cardinal: Card) -> Num
                              # almost five
    mkNum(adverb: AdN, cardinal: Card) -> Num
                              # almost five
  You can also use:
    singularNum, pluralNum : Num
  """
  match __types__(args):
    case ["Numeral"]:
      return w.NumCard(w.NumNumeral(args[0]))
    case ["Digits"]:
      return w.NumCard(w.NumDecimal(w.PosDecimal(args[0])))
    case ["Decimal"]:
      return w.NumCard(w.NumDecimal(args[0]))
    case ["Digit"]:
      return w.NumCard(w.NumNumeral(w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(args[0])))))))
    case ["Card"]:
      return w.NumCard(args[0])
    case ["AdN","Card"]:
      return w.NumCard(w.AdNum(args[0],args[1]))
    case [float]:
      return w.NumCard(w.NumDecimal(float2decimal(args[0])))
    case [int]:
      return w.NumCard(w.NumDecimal(int2decimal(args[0])))
    case types:
      raise __no_match__("mkNum",types)

singularNum = w.NumSg

pluralNum = w.NumPl

def float2decimal(d : float,prec : int=5) -> pgf.Expr:
  dec = int2decimal(int(d))
  f = round(abs(d) % 1,prec)
  while prec > 0 and f % 1 > 0.00001:
    dec = w.IFrac(dec,pgf.ExprFun("D_"+str(int(f*10))))
    f = round((f * 10) % 1,prec)
    prec = prec - 1
  return dec

def mkCard(*args):
  """
  Constructs a cardinal number:
    mkCard(n: int) -> Card
                              # thirty-five, or 1000000
    mkCard(n: float) -> Card  # 3.14
    mkCard(n: Numeral) -> Card
                              # seven
    mkCard(digits: Digits) -> Card
                              # 51
    mkCard(adverb: AdN, cardinal: Card) -> Card
                              # almost fifty
  """
  match __types__(args):
    case ["Numeral"]:
      return w.NumNumeral(args[0])
    case ["Digits"]:
      return w.NumDecimal(w.PosDecimal(args[0]))
    case ["AdN","Card"]:
      return w.AdNum(args[0],args[1])
    case [float]:
      return w.NumDecimal(float2decimal(args[0]))
    case [int]:
      n = args[0]
      if n < 999999:
        return w.NumNumeral(w.num(sub1000000000000(n)))
      else:
        return w.NumDecimal(int2decimal(n))
    case types:
      raise __no_match__("mkCard",types)

def mkOrd(*args):
  """
  Constructs an ordinal number (used in Det)
    mkOrd(n : Numeral) -> Ord
                              # twentieth
    mkOrd(digits: Digits) -> Ord
                              # 51st
    mkOrd(digit: Digit) -> Ord
                              # fifth
    mkOrd(adjective: A) -> Ord
                              # smallest
  """
  match __types__(args):
    case ["Numeral"]:
      return w.OrdNumeral(args[0])
    case ["Digits"]:
      return w.OrdDigits(args[0])
    case ["Digit"]:
      return w.OrdNumeral(w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(args[0]))))))
    case ["A"]:
      return w.OrdSuperl(args[0])
    case types:
      raise __no_match__("mkOrd",types)

def mkAdN(*args):
  """
  mkAdN(adverb: CAdv) -> AdN
  """
  match __types__(args):
    case ["CAdv"]:
      return w.AdnCAdv(args[0])
    case types:
      raise __no_match__("mkAdN",types)

def nd(n):
    return pgf.ExprFun("n"+str(n))

def sub10(n):
    if n == 0:
        return None
    elif n == 1:
        return w.pot01
    else:
        return w.pot0(nd(n))

def sub100(n):
    if n < 10:
        return w.pot0as1(sub10(n))
    elif n == 10:
        return w.pot110
    elif n == 11:
        return w.pot111
    elif n < 20:
        return w.pot1to19(nd(n-10))
    elif n % 10 == 0:
        return w.pot1(nd(n//10))
    else:
        return w.pot1plus(nd(n//10),sub10(n%10))

def sub1000(n):
    if n < 100:
        return w.pot1as2(sub100(n))
    elif n == 100:
        return w.pot21
    elif n % 100 == 0:
        return w.pot2(sub10(n//100))
    else:
        return w.pot2plus(sub10(n//100),sub100(n%100))

def sub1000000(n):
    if n < 1000:
        return w.pot2as3(sub1000(n))
    elif n == 1000:
        return w.pot31
    elif n % 1000 == 0:
        return w.pot3(sub1000(n//1000))
    else:
        return w.pot3plus(sub1000(n//1000),sub1000(n%1000))

def sub1000000000(n):
    if n < 1000000:
        return w.pot3as4(sub1000000(n))
    elif n == 1000000:
        return w.pot41
    elif n % 1000000 == 0:
        return w.pot4(sub1000(n//1000000))
    else:
        return w.pot4plus(sub1000(n//1000000),sub1000000(n%1000000))

def sub1000000000000(n):
    if n < 1000000000:
        return w.pot4as5(sub1000000000(n))
    elif n == 1000000000:
        return w.pot51
    elif n % 1000000000 == 0:
        return w.pot5(sub1000(n//1000000000))
    else:
        return w.pot5plus(sub1000(n//1000000000),sub1000000000(n%1000000000))

def mkNumeral(*args):
  """
  mkNumeral(n: int) -> Numeral
  """
  match __types__(args):
    case [int]:
      return w.num(sub1000000000000(args[0]))
    case ["Sub10"]:
      return w.num(w.pot2as3(w.pot1as2(w.pot0as1(args[0]))))
    case ["Sub100"]:
      return w.num(w.pot2as3(w.pot1as2(args[0])))
    case ["Sub1000"]:
      return w.num(w.pot2as3(args[0]))
    case ["Sub1000","Sub1000"]:
      return w.num(w.pot3plus(args[0],args[1]))
    case types:
      raise __no_match__("mkNumeral",types)

def thousandfoldNumeral(*args):
  match __types__(args):
    case ["Sub1000"]:
      return w.num(w.pot3(args[0]))
    case types:
      raise __no_match__("thousandfoldNumeral",types)

def mkSub1000(*args):
  match __types__(args):
    case ["Sub100"]:
      return w.pot1as2(args[0])
    case ["Sub10"]:
      return w.pot2(args[0])
    case ["Sub10","Sub100"]:
      return w.pot2plus(args[0],args[1])
    case types:
      raise __no_match__("mkSub1000",types)

def mkSub100(*args):
  match __types__(args):
    case ["Sub10"]:
      return w.pot0as1(args[0])
    case ["Sub10","Sub10"]:
      return w.pot1plus(args[0],args[1])
    case types:
      raise __no_match__("mkSub100",types)

def tenfoldSub100(*args):
  match __types__(args):
    case ["Sub10"]:
      return w.pot110(args[0])
    case types:
      raise __no_match__("tenfoldSub100",types)

def int2digits(n : int) -> pgf.Expr:
    expr = pgf.Expr("IDig", [pgf.ExprFun("D_"+str(n % 10))])
    n    = n // 10
    while n != 0:
        expr = pgf.Expr("IIDig", [pgf.ExprFun("D_"+str(n % 10)), expr])
        n    = n // 10
    return expr

def mkDigits(*args):
  """
  Constructs digits:
    mkDigits(n : int) -> Digits
    mkDigits(d : Dig) -> Digits
    mkDigits(d : Dig, digits : Digits) -> Digits
  """
  match __types__(args):
    case [int]:
      return int2digits(args[0])
    case ["Dig"]:
      return w.IDig(args[0])
    case ["Dig","Digits"]:
      return w.IIDig(args[0],args[1])
    case types:
      raise __no_match__("mkDigits",types)

def int2decimal(n : int) -> pgf.Expr:
    if n >= 0:
        sign = "PosDecimal"
    else:
        sign = "NegDecimal"
        n = -n
    expr = int2digits(n)
    return pgf.Expr(sign,[expr])

def mkDecimal(*args):
  """
  Constructs a decimal:
    mkDecimal(n : float) -> Decimal  # 3.14
    mkDecimal(n : int) -> Decimal    # 42
  """
  match __types__(args):
    case [float]:
      return float2decimal(args[0])
    case [int]:
      return int2decimal(args[0])
    case types:
      raise __no_match__("mkDecimal",types)

n0_Dig = w.D_0

n1_Dig = w.D_1

n2_Dig = w.D_2

n3_Dig = w.D_3

n4_Dig = w.D_4

n5_Dig = w.D_5

n6_Dig = w.D_6

n7_Dig = w.D_7

n8_Dig = w.D_8

n9_Dig = w.D_9

def mkCN(*args):
  """
  Constructs a common noun (without determiner)
    mkCN(noun: N) -> CN
                              # house
    mkCN(noun: N2, noun_phrase: NP) -> CN
                              # mother of the king
    mkCN(noun: N3, noun_phrase1: NP, noun_phrase2: NP) -> CN
                              # distance from this city to Paris
    mkCN(noun: N2) -> CN      # mother
    mkCN(noun: N3) -> CN      # distance
    mkCN(adjective: A, noun: N) -> CN
                              # big house
    mkCN(adjective: A, common_noun: CN) -> CN
                              # big blue house
    mkCN(adjectival_phrase: AP, noun: N) -> CN
                              # very big house
    mkCN(adjectival_phrase: AP, common_noun: CN) -> CN
                              # very big blue house
    mkCN(noun: N, relative_sentence: RS) -> CN
                              # man that she loves
    mkCN(common_noun: CN, relative_sentence: RS) -> CN
                              # old man that she loves
    mkCN(noun: N, adverb: Adv) -> CN
                              # house on the hill
    mkCN(common_noun: CN, adverb: Adv) -> CN
                              # big house on the hill
    mkCN(common_noun: CN, sentence: S) -> CN
                              # rule that she sleeps
    mkCN(common_noun: CN, question: QS) -> CN
                              # question if she sleeps
    mkCN(common_noun: CN, verb_phrase: VP) -> CN
                              # reason to sleep
    mkCN(common_noun: CN, embedded_sentence: SC) -> CN
                              # reason to sleep
    mkCN(noun: N, noun_phrase: NP) -> CN
                              # king John
    mkCN(common_noun: CN, noun_phrase: NP) -> CN
                              # old king John
  """
  match __types__(args):
    case ["N"]:
      return w.UseN(args[0])
    case ["N2","NP"]:
      return w.ComplN2(args[0],args[1])
    case ["N3","NP","NP"]:
      return w.ComplN2(w.ComplN3(args[0],args[1]))
    case ["N2"]:
      return w.UseN2
    case ["N3"]:
      return w.UseN2(w.Use2N3(args[0]))
    case ["A","N"]:
      return w.AdjCN(w.PositA(args[0]),w.UseN(args[1]))
    case ["A","CN"]:
      return w.AdjCN(w.PositA(args[0]),args[1])
    case ["AP","N"]:
      return w.AdjCN(args[0],w.UseN(args[1]))
    case ["AP","CN"]:
      return w.AdjCN(args[0],args[1])
    case ["CN","AP"]:
      return w.AdjCN(args[1],args[0])
    case ["N","AP"]:
      return w.AdjCN(args[1],w.UseN(args[0]))
    case ["N","RS"]:
      return w.RelCN(w.UseN(args[0]),args[1])
    case ["CN","RS"]:
      return w.RelCN(args[0],args[1])
    case ["N","Adv"]:
      return w.AdvCN(w.UseN(args[0]),args[1])
    case ["CN","Adv"]:
      return w.AdvCN(args[0],args[1])
    case ["CN","S"]:
      return w.SentCN(args[0],w.EmbedS(args[1]))
    case ["CN","QS"]:
      return w.SentCN(args[0],w.EmbedQS(args[1]))
    case ["CN","VP"]:
      return w.SentCN(args[0],w.EmbedVP(args[1]))
    case ["CN","SC"]:
      return w.SentCN(args[0],args[1])
    case ["N","NP"]:
      return w.ApposCN(w.UseN(args[0]),args[1])
    case ["CN","NP"]:
      return w.ApposCN(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjCN(args[0],mkList(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case types:
      raise __no_match__("mkCN",types)

def mkAP(*args):
  """
  Constructs an adjectival phrase
    mkAP(adjective: A) -> AP
                              # warm
    mkAP(adjective: A, noun_phrase: NP) -> AP
                              # warmer than Paris
    mkAP(adjective: A2, noun_phrase: NP) -> AP
                              # married to her
    mkAP(adjective: A2) -> AP # married
    mkAP(adjectival_phrase: AP, sentence: S) -> AP
                              # it is good that she sleeps
    mkAP(adjectival_phrase: AP, question: QS) -> AP
                              # it is uncertain who sleeps
    mkAP(adjectival_phrase: AP, verb_phrase: VP) -> AP
                              # she is ready to sleep
    mkAP(adjectival_phrase: AP, embeded_phrase: SC) -> AP
                              # she is ready to sleep
    mkAP(adverb: AdA, adjective: A) -> AP
                              # very old
    mkAP(adverb: AdA, adjectival_phrase: AP) -> AP
                              # very very old
    mkAP(conj: Conj, adjectival_phrase1: AP, adjectival_phrase2: AP) -> AP
                              # old or young
    mkAP(conj: Conj, list: list[AP]) -> AP
                              # old, big and warm
    mkAP(ordinal: Ord) -> AP
                              # oldest
    mkAP(adverb: CAdv, adjectival_phrase: AP, noun_phrase: NP) -> AP
                              # as old as she
  """
  match __types__(args):
    case ["A"]:
      return w.PositA(args[0])
    case ["A","NP"]:
      return w.ComparA(args[0],args[1])
    case ["A2","NP"]:
      return w.ComplA2(args[0],args[1])
    case ["A2"]:
      return w.UseA2(args[0])
    case ["V2"]:
      return w.PastPartAP(w.SlashV2a(args[0]))
    case ["VPSlash"]:
      return w.PastPartAP(args[0])
    case ["V"]:
      return w.PresPartAP(w.UseV(args[0]))
    case ["VP"]:
      return w.PastPartAP(args[0])
    case ["AP","S"]:
      return w.SentAP(args[0],w.EmbedS(args[1]))
    case ["AP","QS"]:
      return w.SentAP(args[0],w.EmbedQS(args[1]))
    case ["AP","VP"]:
      return w.SentAP(args[0],w.EmbedVP(args[1]))
    case ["AP","SC"]:
      return w.SentAP(args[0],args[1])
    case ["AdA","A"]:
      return w.AdAP(args[0],w.PositA(args[1]))
    case ["AdA","AP"]:
      return w.AdAP(args[0],args[1])
    case ["Conj","AP","AP"]:
      return w.ConjAP(args[0],w.BaseAP(args[1],args[2]))
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjAP(args[0],mkList(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["CAdv","AP","NP"]:
      return w.CAdvAP(args[0],args[1],args[2],args[3])
    case types:
      raise __no_match__("mkAP",types)

def reflAP(*args):
  """
  reflAP(adjective: A2) -> AP # married to itself
  """
  match __types__(args):
    case ["A2"]:
      return w.ReflA2(args[0],args[1])
    case types:
      raise __no_match__("reflAP",types)

def comparAP(*args):
  """
  comparAP(adjective: A) -> AP
  """
  match __types__(args):
    case ["A"]:
      return w.UseComparA(args[0])
    case types:
      raise __no_match__("comparAP",types)

def mkAdv(*args):
  """
  Constructs an adverb
    mkAdv(adjective: A) -> Adv
                              # warmly
    mkAdv(preposition: Prep, noun_phrase: NP) -> Adv
                              # in the house
    mkAdv(subj: Subj, sentence: S) -> Adv
                              # when she sleeps
    mkAdv(modifier: CAdv, adjective: A, noun_phrase: NP) -> Adv
                              # more warmly than he
    mkAdv(modifier: CAdv, adjective: A, sentence: S) -> Adv
                              # more warmly than he runs
    mkAdv(modifier: AdA, adverb: Adv) -> Adv
                              # very warmly
    mkAdv(conj: Conj, adverb1: Adv, adverb2: Adv) -> Adv
                              # here and now
    mkAdv(conj: Conj, list: list[Adv]) -> Adv
                              # with her, here and now
  """
  match __types__(args):
    case ["A"]:
      return w.PositAdvAdj(args[0])
    case ["Prep","NP"]:
      return w.PrepNP(args[0],args[1])
    case ["Subj","S"]:
      return w.SubjS(args[0],args[1])
    case ["CAdv","A","NP"]:
      return w.ComparAdvAdj
    case ["CAdv","A","S"]:
      return w.ComparAdvAdjS
    case ["AdA","Adv"]:
      return w.AdAdv(args[0],args[1])
    case ["Conj","Adv","Adv"]:
      return w.ConjAdv(args[0],w.BaseAdv(args[1],args[2]))
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjAdv(args[0],mkList(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["LN"]:
      return w.InLN(args[0])
    case types:
      raise __no_match__("mkAdv",types)

def mkQS(*args):
  """
  mkQS(tense: Tense, anteriority: Ant, polarity: Pol, question: QCl) -> QS
                              # who wouldn't have slept
  mkQS(clause: Cl) -> QS     # does she sleep
  """
  match __types__(args):
    case ["QCl"]:
      return w.TUseQCl(w.TPres,w.ASimul,w.PPos)
    case ["Tense","QCl"]:
      return w.TUseQCl(args[0],w.ASimul,w.PPos)
    case ["Ant","QCl"]:
      return w.TUseQCl(w.TPres,args[0],w.PPos)
    case ["Pol","QCl"]:
      return w.TUseQCl(w.TPres,w.ASimul,args[0])
    case ["Tense","Ant","QCl"]:
      return w.TUseQCl(args[0],args[1],w.PPos)
    case ["Tense","Pol","QCl"]:
      return w.TUseQCl(args[0],w.ASimul,args[1])
    case ["Ant","Pol","QCl"]:
      return w.TUseQCl(w.TPres,args[0],args[1])
    case ["Tense","Ant","Pol","QCl"]:
      return w.TUseQCl
    case ["Temp","Pol","QCl"]:
      return w.UseQCl(args[0],args[1],args[2])
    case ["Cl"]:
      return w.TUseQCl(w.TPres,w.ASimul,w.PPos,w.QuestCl(args[0]))
    case types:
      raise __no_match__("mkQS",types)

def mkQCl(*args):
  """
  Constructs a question clause, with all tenses
    mkQCl(clause: Cl) -> QCl  # does she sleep
    mkQCl(interogative: IP, verb_phrase: VP) -> QCl
                              # who always sleeps
    mkQCl(interogative: IP, verb: V) -> QCl
                              # who sleeps
    mkQCl(interogative: IP, verb: V2, object: NP) -> QCl
                              # who loves her
    mkQCl(interogative: IP, verb: V3, object1: NP, object2: NP) -> QCl
                              # who sends it to her
    mkQCl(interogative: IP, verb: VV, verb_phrse: VP) -> QCl
                              # who wants to sleep
    mkQCl(interogative: IP, verb: VS, sentence: S) -> QCl
                              # who says that I sleep
    mkQCl(interogative: IP, verb: VQ, question: QS) -> QCl
                              # who wonders who sleeps
    mkQCl(interogative: IP, verb: VA, adjective: A) -> QCl
                              # who becomes old
    mkQCl(interogative: IP, verb: VA, adjectival_phrase: AP) -> QCl
                              # who becomes very old
    mkQCl(interogative: IP, verb: V2A, object: NP, adjective: A) -> QCl
                              # who paints it red
    mkQCl(interogative: IP, verb: V2A, object: NP, adjectival_phrase: AP) -> QCl
                              # who paints it very red
    mkQCl(interogative: IP, verb: V2S, object: NP, sentence: S) -> QCl
                              # who answers to him that we sleep
    mkQCl(interogative: IP, verb: V2Q, object: NP, question: QS) -> QCl
                              # who asks him who sleeps
    mkQCl(interogative: IP, verb: V2V, object: NP, verb_phrse: VP) -> QCl
                              # who begs him to sleep
    mkQCl(interogative: IP, adjective: A) -> QCl
                              # who is old
    mkQCl(interogative: IP, adjective: A, object: NP) -> QCl
                              # who is older than he
    mkQCl(interogative: IP, adjective: A2, object: NP) -> QCl
                              # who is married to him
    mkQCl(interogative: IP, adjectival_phrase: AP) -> QCl
                              # who is very old
    mkQCl(interogative: IP, object: NP) -> QCl
                              # who is the woman
    mkQCl(interogative: IP, noun: N) -> QCl
                              # who is a woman
    mkQCl(interogative: IP, common_noun: CN) -> QCl
                              # who is an old woman
    mkQCl(interogative: IP, adverb: Adv) -> QCl
                              # who is here
    mkQCl(interogative: IP, object: NP, verb: V2) -> QCl
                              # who is her
    mkQCl(interogative: IP, clause: ClSlash) -> QCl
                              # whom does she love today
    mkQCl(adverb: IAdv, clause: Cl) -> QCl
                              # why does she sleep
    mkQCl(preposition: Prep, interogative: IP, clause: Cl) -> QCl
                              # with whom does she sleep
    mkQCl(adverb: IAdv, object: NP) -> QCl
                              # where is she
    mkQCl(comp: IComp, object: NP) -> QCl
                              # who is this man
    mkQCl(interogative: IP, question) -> QCl
                              # which city is there
  """
  match __types__(args):
    case ["Cl"]:
      return w.QuestCl(args[0])
    case ["IP","VP"]:
      return w.QuestVP(args[0],args[1])
    case ["IP","V"]:
      return w.QuestVP(args[0],w.UseV(args[1]))
    case ["IP","V2","NP"]:
      return w.QuestVP(args[0],w.ComplV2(args[1],args[2]))
    case ["IP","V3","NP","NP"]:
      return w.QuestVP(args[0],w.ComplSlash(w.Slash3V3(args[1],args[3]),args[2]))
    case ["IP","VV","VP"]:
      return w.QuestVP(args[0],w.ComplVV(args[1],w.ASimul,w.PPos,args[2]))
    case ["IP","VS","S"]:
      return w.QuestVP(args[0],w.ComplVS(args[1],args[2]))
    case ["IP","VQ","QS"]:
      return w.QuestVP(args[0],w.ComplVQ(args[1],args[2]))
    case ["IP","VA","A"]:
      return w.QuestVP(args[0],w.ComplVA(args[1],w.PositA(args[2])))
    case ["IP","VA","AP"]:
      return w.QuestVP(args[0],w.ComplVA(args[1],args[2]))
    case ["IP","V2A","NP","A"]:
      return w.QuestVP(args[0],w.ComplV2A(args[1],args[2],w.PositA(args[3])))
    case ["IP","V2A","NP","AP"]:
      return w.QuestVP(args[0],w.ComplV2A(args[1],args[2],args[3]))
    case ["IP","V2S","NP","S"]:
      return w.QuestVP(args[0],w.ComplSlash(w.SlashV2S(args[1],args[3]),args[2]))
    case ["IP","V2Q","NP","QS"]:
      return w.QuestVP(args[0],w.ComplSlash(w.SlashV2Q(args[1],args[3]),args[2]))
    case ["IP","V2V","NP","VP"]:
      return w.QuestVP(args[0],w.ComplSlash(w.SlashV2V(args[1],args[3]),args[2]))
    case ["IP","A"]:
      return w.QuestVP(args[0],w.UseComp(w.CompAP(w.PositA(args[1]))))
    case ["IP","A","NP"]:
      return w.QuestVP(args[0],w.UseComp(w.CompAP(w.ComparA(args[1],args[2]))))
    case ["IP","A2","NP"]:
      return w.QuestVP(args[0],w.UseComp(w.CompAP(w.ComplA2(args[1],args[2]))))
    case ["IP","AP"]:
      return w.QuestVP(args[0],w.UseComp(w.CompAP(args[1])))
    case ["IP","NP"]:
      return w.QuestVP(args[0],w.UseComp(w.CompNP(args[1])))
    case ["IP","N"]:
      return w.QuestVP(args[0],w.UseComp(w.CompCN(w.UseN(args[1]))))
    case ["IP","CN"]:
      return w.QuestVP(args[0],w.UseComp(w.CompCN(args[1])))
    case ["IP","Adv"]:
      return w.QuestVP(args[0],w.UseComp(w.CompAdv(args[1])))
    case ["IP","NP","V2"]:
      return w.QuestSlash(args[0],w.SlashVP(args[1],w.SlashV2a(args[2])))
    case ["IP","ClSlash"]:
      return w.QuestSlash(args[0],args[1])
    case ["IAdv","Cl"]:
      return w.QuestIAdv(args[0],args[1])
    case ["Prep","IP","Cl"]:
      return w.QuestIAdv(w.PrepIP(args[0],args[1]))
    case ["IAdv","NP"]:
      return w.QuestIComp(w.CompIAdv(args[0]))
    case ["IComp","NP"]:
      return w.QuestIComp(args[0])
    case ["IP"]:
      return w.ExistIP(args[0])
    case types:
      raise __no_match__("mkQCl",types)

def mkIComp(*args):
  """
  Constructs an interrogative complement of a copula
    mkIComp(adverb: IAdv) -> IComp
                              # where (is it)
    mkIComp(interogative: IP) -> IComp
                              # who (is it)
  """
  match __types__(args):
    case ["IAdv"]:
      return w.CompIAdv(args[0])
    case ["IP"]:
      return w.CompIP(args[0])
    case types:
      raise __no_match__("mkIComp",types)

def mkIP(*args):
  """
  Constructs an interrogative pronoun
    mkIP(determiner: IDet, common_noin: CN) -> IP
                              # which five big cities
    mkIP(determiner: IDet, noun: N) -> IP
                              # which five cities
    mkIP(determiner: IDet) -> IP
                              # which five
    mkIP(quantifier: IQuant, common_noun: CN) -> IP	
                              # which big city
    mkIP(quantifier: IQuant, number: Num, common_noun: CN) -> IP	
                              # which five big cities
    mkIP(quantifier: IQuant, noun: N) -> IP
                              # which city
    mkIP(interogative: IP, adverb: Adv) -> IP
                              # who in Paris
  You can also use:
    what_IP, who_IP : IP
  """
  match __types__(args):
    case ["IDet","CN"]:
      return w.IdetCN(args[0],args[1])
    case ["IDet","N"]:
      return w.IdetCN(args[0],w.UseN(args[1]))
    case ["IDet"]:
      return w.IdetIP(args[0])
    case ["IQuant","CN"]:
      return w.IdetCN(w.IdetQuant(args[0],w.NumSg),args[1])
    case ["IQuant","Num","CN"]:
      return w.IdetCN(w.IdetQuant(args[0],args[1]),args[2])
    case ["IQuant","N"]:
      return w.IdetCN(w.IdetQuant(args[0],w.NumSg),w.UseN(args[1]))
    case ["IP","Adv"]:
      return w.AdvIP(args[0],args[1])
    case types:
      raise __no_match__("mkIP",types)

what_IP = w.whatSg_IP

who_IP = w.whoSg_IP

def mkIAdv(*args):
  """
  mkIAdv(preposition: Prep, interogative: IP) -> IAdv
                              # in which city
  mkIAdv(inter_adverb: IAdv, adverb: Adv) -> IAdv
                              # where in Paris
  """
  match __types__(args):
    case ["Prep","IP"]:
      return w.PrepIP(args[0],args[1])
    case ["IAdv","Adv"]:
      return w.AdvIAdv(args[0],args[1])
    case types:
      raise __no_match__("mkIAdv",types)

def mkIDet(*args):
  """
  Constructs an interrogative determiner
    mkIDet(quantifuer: IQuant, number: Num) -> IDet	
                              # which houses
    mkIDet(quantifuer: IQuant) -> IDet
                              # which house
  You can also use:
    whichPl_IDet, whichSg_IDet, which_IDet : IDet
  """
  match __types__(args):
    case ["IQuant","Num"]:
      return w.IdetQuant(args[0],args[1])
    case ["IQuant"]:
      return w.IdetQuant(args[0],w.NumSg)
    case types:
      raise __no_match__("mkIDet",types)

whichPl_IDet = w.IdetQuant(w.which_IQuant,w.NumPl)

whichSg_IDet = w.IdetQuant(w.which_IQuant,w.NumSg)

which_IDet = whichSg_IDet

def mkRS(*args):
  """
  Constructs a relative sentence
    mkRS(clause: RCl) -> RS
                              # woman that sleeps
    mkRS(tense: Tense, clause: RCl) -> RS
                              # woman that will sleep
    mkRS(anteriority: Ant, clause: RCl) -> RS
                              # woman that have slept
    mkRS(polarity: Pol, clause: RCl) -> RS
                              # woman that does not sleep
    mkRS(tense: Tense, anteriority: Ant, clause: RCl) -> RS
                              # woman that would have slept
    mkRS(tense: Tense, polarity: Pol, clause: RCl) -> RS
                              # woman that willn't sleep
    mkRS(temp: Temp, polarity: Pol, clause: RCl) -> RS
                              # that wouldn't have slept
    mkRS(conjunction: Conj, relative1: RS, relative2: RS) -> RS	
                              # woman that sleeps or that we love
    mkRS(conjunction: Conj, list: list[RS]) -> RS
                              # who sleeps, whom I see and who sleeps
  """
  match __types__(args):
    case ["RCl"]:
      return w.UseRCl(w.TTAnt(w.TPres,w.ASimul),w.PPos,args[0])
    case ["Tense","RCl"]:
      return w.UseRCl(w.TTAnt(args[0],w.ASimul),w.PPos,args[1])
    case ["Ant","RCl"]:
      return w.UseRCl(w.TTAnt(w.TPres,args[0]),w.PPos,args[1])
    case ["Pol","RCl"]:
      return w.UseRCl(w.TTAnt(w.TPres,w.ASimul),args[0],args[1])
    case ["Tense","Ant","RCl"]:
      return w.UseRCl(w.TTAnt(args[0],args[1]),w.PPos,args[2])
    case ["Tense","Pol","RCl"]:
      return w.UseRCl(w.TTAnt(args[0],w.ASimul),args[1],args[2])
    case ["Ant","Pol","RCl"]:
      return w.UseRCl(w.TTAnt(w.TPres,args[0]),args[1],args[2])
    case ["Tense","Ant","Pol","RCl"]:
      return w.UseRCl(w.TTAnt(args[0],args[1]),args[2],args[3])
    case ["Temp","Pol","RCl"]:
      return w.UseRCl(args[0],args[1],args[2])
    case ["Conj","RS","RS"]:
      return w.ConjRS(args[0],w.BaseRS(args[1],args[2]))
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjRS(args[0],mkList(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case types:
      raise __no_match__("mkRS",types)

def mkRCl(*args):
  """
  Constructs a relative clause, with all tenses
    mkRCl(pronoun: RP, verb_phrase: VP) -> RCl	
                              # woman that always sleeps
    mkRCl(pronoun: RP, verb: V) -> RCl	
                              # woman that sleeps
    mkRCl(pronoun: RP, verb: V2, object: NP) -> RCl	
                              # woman that loves him
    mkRCl(pronoun: RP, verb: V3, object1: NP, object2: NP) -> RCl	
                              # woman that sends it to him
    mkRCl(pronoun: RP, verb: VV, verb_phrase: VP) -> RCl	
                              # woman that wants to sleep
    mkRCl(pronoun: RP, verb: VS, sentence: S) -> RCl	
                              # woman that says that I sleep
    mkRCl(pronoun: RP, verb: VQ, question: QS) -> RCl	
                              # woman that wonders who sleeps
    mkRCl(pronoun: RP, verb: VA, adjective: A) -> RCl	
                              # woman that becomes old
    mkRCl(pronoun: RP, verb: VA, adjectival_phrase: AP) -> RCl	
                              # woman that becomes very old
    mkRCl(pronoun: RP, verb: V2A, object: NP, adjective: A) -> RCl	
                              # woman that paints it red
    mkRCl(pronoun: RP, verb: V2A, noun_phrase: NP, adjectival_prase: AP) -> RCl	
                              # woman that paints it very red
    mkRCl(pronoun: RP, verb: V2S, noun_phrase: NP, sentence: S) -> RCl	
                              # woman that answers to him that we sleep
    mkRCl(pronoun: RP, verb: V2Q, noun_phrase: NP, question: QS) -> RCl	
                              # woman that asks him who sleeps
    mkRCl(pronoun: RP, verb: V2V, noun_phrase: NP, verb_phrase: VP) -> RCl	
                              # woman that begs him to sleep
    mkRCl(pronoun: RP, adjective: A) -> RCl	
                              # woman that is old
    mkRCl(pronoun: RP, adjective: A, noun_phrase: NP) -> RCl	
                              # woman that is older than he
    mkRCl(pronoun: RP, adjective: A2, noun_phrase: NP) -> RCl	
                              # woman that is married to him
    mkRCl(pronoun: RP, adjectival_phrase: AP) -> RCl	
                              # woman that is very old
    mkRCl(pronoun: RP, noun_phrase: NP) -> RCl	
                              # woman that is the woman
    mkRCl(pronoun: RP, noun: N) -> RCl	
                              # student that is a woman
    mkRCl(pronoun: RP, common_noun: CN) -> RCl	
                              # student that is an old woman
    mkRCl(pronoun: RP, adverb: Adv) -> RCl
                              # woman that is here
    mkRCl(pronoun: RP, noun_phrase: NP, verb: V2) -> RCl	
                              # woman that we love
    mkRCl(pronoun: RP, clause: ClSlash) -> RCl	
                              # woman that she loves today
    mkRCl(clause: Cl) -> RCl  # such that she loves him
  """
  match __types__(args):
    case ["RP","VP"]:
      return w.RelVP(args[0],args[1])
    case ["RP","V"]:
      return w.RelVP(args[0],w.UseV(args[1]))
    case ["RP","V2","NP"]:
      return w.RelVP(args[0],w.ComplV2(args[1],args[2]))
    case ["RP","V3","NP","NP"]:
      return w.RelVP(args[0],w.ComplSlash(w.Slash3V3(args[1],args[3]),args[2]))
    case ["RP","VV","VP"]:
      return w.RelVP(args[0],w.ComplVV(args[1],w.ASimul,w.PPos,args[2]))
    case ["RP","VS","S"]:
      return w.RelVP(args[0],w.ComplVS(args[1],args[2]))
    case ["RP","VQ","QS"]:
      return w.RelVP(args[0],w.ComplVQ(args[1],args[2]))
    case ["RP","VA","A"]:
      return w.RelVP(args[0],w.ComplVA(args[1],w.PositA(args[2])))
    case ["RP","VA","AP"]:
      return w.RelVP(args[0],w.ComplVA(args[1],args[2]))
    case ["RP","V2A","NP","A"]:
      return w.RelVP(args[0],w.ComplV2A(args[1],args[2],w.PositA(args[3])))
    case ["RP","V2A","NP","AP"]:
      return w.RelVP(args[0],w.ComplV2A(args[1],args[2],args[3]))
    case ["RP","V2S","NP","S"]:
      return w.RelVP(args[0],w.ComplSlash(w.SlashV2S(args[1],args[3]),args[2]))
    case ["RP","V2Q","NP","QS"]:
      return w.RelVP(args[0],w.ComplSlash(w.SlashV2Q(args[1],args[3]),args[2]))
    case ["RP","V2V","NP","VP"]:
      return w.RelVP(args[0],w.ComplSlash(w.SlashV2V(args[1],args[3]),args[2]))
    case ["RP","A"]:
      return w.RelVP(args[0],w.UseComp(w.CompAP(w.PositA(args[1]))))
    case ["RP","A","NP"]:
      return w.RelVP(args[0],w.UseComp(w.CompAP(w.ComparA(args[1],args[2]))))
    case ["RP","A2","NP"]:
      return w.RelVP(args[0],w.UseComp(w.CompAP(w.ComplA2(args[1],args[2]))))
    case ["RP","AP"]:
      return w.RelVP(args[0],w.UseComp(w.CompAP(args[1])))
    case ["RP","NP"]:
      return w.RelVP(args[0],w.UseComp(w.CompNP(args[1])))
    case ["RP","N"]:
      return w.RelVP(args[0],w.UseComp(w.CompCN(w.UseN(args[1]))))
    case ["RP","CN"]:
      return w.RelVP(args[0],w.UseComp(w.CompCN(args[1])))
    case ["RP","Adv"]:
      return w.RelVP(args[0],w.UseComp(w.CompAdv(args[1])))
    case ["RP","NP","V2"]:
      return w.RelSlash(args[0],w.SlashVP(args[1],w.SlashV2a(args[2])))
    case ["RP","ClSlash"]:
      return w.RelSlash(args[0],args[1])
    case ["Cl"]:
      return w.RelCl(args[0])
    case types:
      raise __no_match__("mkRCl",types)

which_RP = w.IdRP

def mkRP(*args):
  """
  Constructs a relative pronoun:
    mkRP(preposition: Prep, noun_phrase: NP, pronoun: RP) -> RP
                              # all the cities in which
  You can also use:
    which_RP : RP             # which 
  """
  match __types__(args):
    case ["Prep","NP","RP"]:
      return w.FunRP(args[0],args[1],args[2])
    case types:
      raise __no_match__("mkRP",types)

def mkSSlash(*args):
  """
  mkSSlash(temp: Temp, polarity: Pol, clause: ClSlash) -> SSlash
  """
  match __types__(args):
    case ["Temp","Pol","ClSlash","SSlash"]:
      return w.UseSlash(args[0],args[1],args[2])
    case types:
      raise __no_match__("mkSSlash",types)

def mkClSlash(*args):
  """
  Constructs a clause with a missing noun phrase
    mkClSlash(subject: NP, verb: VPSlash) -> ClSlash
                              # whom does she see
    mkClSlash(subject: NP, verb: V2) -> ClSlash
                              # whom does she see
    mkClSlash(subject: NP, verb: VV -> V2) -> ClSlash
                              # whom does she want to see
    mkClSlash(subject: Cl, preposition: Prep) -> ClSlash
                              # whom does she sleep with
    mkClSlash(subject: ClSlash, adverb: Adv) -> ClSlash
                              # whom does she see today
    mkClSlash(subject: NP, verb: VS, sentence: SSlash) -> ClSlash
                              # whom does she know that we hadn't seen
  """
  match __types__(args):
    case ["NP","VPSlash"]:
      return w.SlashVP(args[0],args[1])
    case ["NP","V2"]:
      return w.SlashVP(args[0],w.SlashV2a(args[1]))
    case ["NP","VV","V2"]:
      return w.SlashVP(args[0],w.SlashVV(args[1],w.SlashV2a(args[2])))
    case ["Cl","Prep"]:
      return w.SlashPrep(args[0],args[1])
    case ["ClSlash","Adv"]:
      return w.AdvSlash(args[0],args[1])
    case ["NP","VS","SSlash"]:
      return w.SlashVS(args[0],args[1],args[2])
    case types:
      raise __no_match__("mkClSlash",types)

def mkVPSlash(*args):
  """
  Constructs a verb phrase with missing complement
    mkVPSlash(verb: V2) -> VPSlash
                              # whom does she see
    mkVPSlash(verb: V3, object: NP) -> VPSlash
                              # whom does she send it to
    mkVPSlash(verb: V2A, adjectival_phrase: AP) -> VPSlash
                              # whom does she paint red
    mkVPSlash(verb: V2Q, question: QS) -> VPSlash
                              # whom does she ask where I sleep
    mkVPSlash(verb: V2S, sentence: S) -> VPSlash
                              # whom does she answer that I sleep to
    mkVPSlash(verb: V2V, verb_phrase: VP) -> VPSlash
                              # whom does she beg to sleep
    mkVPSlash(verb: VV, verb_phrase: VPSlash) -> VPSlash
                              # whom does she want to see
    mkVPSlash(verb: V2V, noun_phrase: NP, verb_phrase: VPSlashVPSlash) -> VPSlash
                              # whom does she beg me to see
  """
  match __types__(args):
    case ["V2"]:
      return w.SlashV2a(args[0])
    case ["V3","NP"]:
      return w.Slash2V3(args[0],args[1])
    case ["V2A","AP"]:
      return w.SlashV2A(args[0],args[1])
    case ["V2Q","QS"]:
      return w.SlashV2Q(args[0],args[1])
    case ["V2S","S"]:
      return w.SlashV2S(args[0],args[1])
    case ["V2V","VP"]:
      return w.SlashV2V(args[0],args[1],args[2],args[3])
    case ["VV","VPSlash"]:
      return w.SlashVV(args[0],args[1],args[2],args[3])
    case ["V2V","NP","VPSlash"]:
      return w.SlashV2VNP(args[0],args[1],args[2],args[3],args[4])
    case ["VPSlash","Adv"]:
      return w.AdvVPSlash(args[0],args[1])
    case types:
      raise __no_match__("mkVPSlash",types)

def mkList(*args):
  types = __types__(args)
  if len(types) >= 2:
    cat   = types[0]
    count = types.count(types[0])
    base  = pgf.ExprFun("Base"+cat)
    cons  = pgf.ExprFun("Cons"+cat)

    if count == len(types):
      expr = base(args[-2],args[-1])
      i    = len(args)-3
    elif count == len(types)-1 and types[-1] == "List"+cat:
      expr = args[-1]
      i    = len(args)-2
    else:
      raise __no_match__("mkList"+cat,types)

    while i >= 0:
      expr = cons(args[i],expr)
      i = i - 1

    return expr
  else:
    raise __no_match__("mkList"+cat,types)

def mkUttImpSg(*args):
  """
  mkUttImpSg(polarity: imperative: Imp) -> Utt
  """
  match __types__(args):
    case ["Pol","Imp"]:
      return w.UttImpSg(args[0],arg[1])
    case types:
      raise __no_match__("mkUttImpSg",types)

def mkUttImpPl(*args):
  """
  mkUttImpPl(polarity: imperative: Imp) -> Utt
  """
  match __types__(args):
    case ["Pol","Imp"]:
      return w.UttImpPl(args[0],arg[1])
    case types:
      raise __no_match__("mkUttImpPl",types)
      
def mkUttImpPol(*args):
  """
  mkUttImpPol(polarity: imperative: Imp) -> Utt
  """
  match __types__(args):
    case ["Pol","Imp"]:
      return w.UttImpPol(args[0],arg[1])
    case types:
      raise __no_match__("mkUttImpPol",types)

n1_Digits = w.IDig(w.D_1)

n2_Digits = w.IDig(w.D_2)

n3_Digits = w.IDig(w.D_3)

n4_Digits = w.IDig(w.D_4)

n5_Digits = w.IDig(w.D_5)

n6_Digits = w.IDig(w.D_6)

n7_Digits = w.IDig(w.D_7)

n8_Digits = w.IDig(w.D_8)

n9_Digits = w.IDig(w.D_9)

n10_Digits = w.IIDig(w.D_1,w.IDig(w.D_0))

n20_Digits = w.IIDig(w.D_2,w.IDig(w.D_0))

n100_Digits = w.IIDig(w.D_1,w.IIDig(w.D_0,w.IDig(w.D_0)))

n1000_Digits = w.IIDig(w.D_1,w.IIDig(w.D_0,w.IIDig(w.D_0,w.IDig(w.D_0))))

n1_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot01))))

n2_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n2)))))

n3_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n3)))))

n4_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n4)))))

n5_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n5)))))

n6_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n6)))))

n7_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n7)))))

n8_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n8)))))

n9_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(w.n9)))))

n10_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot110)))

n20_Numeral = w.num(w.pot2as3(w.pot1as2(w.pot1(w.n2))))

n100_Numeral = w.num(w.pot2as3(w.pot2(w.pot01)))

n1000_Numeral = w.num(w.pot3(w.pot1as2(w.pot0as1(w.pot01))))
