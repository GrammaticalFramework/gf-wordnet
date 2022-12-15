import pgf
import wordnet as w

def __types__(args):
    types = []
    for arg in args:
        if isinstance(arg,pgf.Expr):
            (fun,_) = arg.unpack()
            types.append(w.__pgf__.functionType(fun).cat)
        else:
            types.append(type(arg))
    return types

def __no_match__(name,args):
    raise TypeError("no overload for function "+name+" with argument types "+" ".join(map(str,args)))

fullStopPunct = w.FullStop
questMarkPunct = w.QuestMark
exclMarkPunct = w.ExclMark

def mkPhr(*args):
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
      __no_match__("mkPhr",types)

def mkPConj(*args):
  match __types__(args):
    case ["Conj"]:
      return w.PConjConj(args[0])
    case types:
      __no_match__("mkPConj",types)

noPConj = w.NoPConj

def mkVoc(*args):
  match __types__(args):
    case ["NP"]:
      return w.VocNP(args[0])
    case types:
      __no_match__("mkVoc",types)

noVoc = w.NoVoc

def mkUtt(*args):
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
      return w.UttAP(args[0],args[1])
    case ["Card"]:
      return w.UttCard(args[0])
    case types:
      __no_match__("mkUtt",types)

def lets_Utt(*args):
  match __types__(args):
    case ["VP"]:
      return w.ImpPl1(args[0])
    case types:
      __no_match__("lets_Utt",types)

positivePol = w.PPos
negativePol = w.PNeg

simultaneousAnt = w.ASimul
anteriorAnt = w.AAnter

presentTense = w.TPres
pastTense = w.TPast
futureTense = w.TFut
conditionalTense = w.TCond

def mkTemp(*args):
  match __types__(args):
    case ["Tense","Ant"]:
      return w.TTAnt(args[0],args[1])
    case types:
      __no_match__("mkTemp",types)

def mkS(*args):
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
    case ["Conj","ListS"]:
      return w.ConjS(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjS(args[0],mkListS(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["Adv","S"]:
      return w.AdvS(args[0],args[1])
    case types:
      __no_match__("mkS",types)

def mkCl(*args):
  match __types__(args):
    case ["NP","V"]:
      return w.PredVP(args[0],w.UseV(args[1]))
    case ["NP","V2","NP"]:
      return w.PredVP(args[0],w.ComplV2(args[1],args[2]))
    case ["NP","V3","NP","NP"]:
      return w.PredVP(args[0],w.ComplV3(args[1],args[2],args[3]))
    case ["NP","VV","VP"]:
      return w.PredVP(args[0],w.ComplVV(args[1],args[2]))
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
      __no_match__("mkCl",types)

def genericCl(*args):
  match __types__(args):
    case ["VP"]:
      return w.GenericCl(args[0])
    case types:
      __no_match__("genericCl",types)

def mkVP(*args):
  match __types__(args):
    case ["V"]:
      return w.UseV(args[0])
    case ["V2","NP"]:
      return w.ComplSlash(w.SlashV2a(args[0]),args[1])
    case ["V3","NP","NP"]:
      return w.ComplV3
    case ["VV","VP"]:
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
      return w.ReflVP
    case ["Comp"]:
      return w.UseComp(args[0])
    case types:
      __no_match__("mkVP",types)

def reflexiveVP(*args):
  match __types__(args):
    case ["V2"]:
      return w.ReflVP(w.SlashV2a(args[0]))
    case ["VPSlash"]:
      return w.ReflVP
    case types:
      __no_match__("reflexiveVP",types)

def passiveVP(*args):
  match __types__(args):
    case ["V2"]:
      return w.PassV2
    case ["V2","NP"]:
      return w.AdvVP(w.PassV2(args[0]),w.PrepNP(w.by8agent_Prep,args[1]))
    case types:
      __no_match__("passiveVP",types)

def progressiveVP(*args):
  match __types__(args):
    case ["VP"]:
      return w.ProgrVP(args[0])
    case types:
      __no_match__("progressiveVP",types)

def mkComp(*args):
  match __types__(args):
    case ["AP"]:
      return w.CompAP(args[0])
    case ["NP"]:
      return w.CompNP(args[0])
    case ["Adv"]:
      return w.CompAdv(args[0])
    case types:
      __no_match__("mkComp",types)

def mkSC(*args):
  match __types__(args):
    case ["S"]:
      return w.EmbedS(args[0])
    case ["QS"]:
      return w.EmbedQS(args[0])
    case ["VP"]:
      return w.EmbedVP(args[0],args[1],args[2],args[3])
    case types:
      __no_match__("mkSC",types)

def mkImp(*args):
  match __types__(args):
    case ["VP"]:
      return w.ImpVP(args[0])
    case ["V"]:
      return w.ImpVP(w.UseV(args[0]))
    case ["V2","NP"]:
      return w.ImpVP(w.ComplV2(args[0],args[1]))
    case types:
      __no_match__("mkImp",types)

def mkNP(*args):
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
    case ["Numeral","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0]))),args[1])
    case ["Numeral","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0]))),w.UseN(args[1]))
    case ["Digits","CN"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumDigits(args[0]))),args[1])
    case ["Digits","N"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumDigits(args[0]))),w.UseN(args[1]))
    case ["Digit","CN","NP"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(args[0])))))))),args[1])
    case ["Digit","N","NP"]:
      return w.DetCN(w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(args[0])))))))),w.UseN(args[1]))
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
    case ["Conj","ListNP"]:
      return w.ConjNP(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjNP(args[0],mkListNP(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["Quant","CN"]:
      return w.DetCN(w.DetQuant(args[0],w.NumSg),args[1])
    case types:
      __no_match__("mkNP",types)

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
      return w.DetQuant(w.IndefArt,w.NumCard(w.NumDigits(args[0])))
    case ["Numeral"]:
      return w.DetQuant(w.IndefArt,w.NumCard(w.NumNumeral(args[0])))
    case ["Pron"]:
      return w.DetQuant(w.PossPron(args[0]),w.NumSg)
    case ["Pron","Num"]:
      return w.DetQuant(w.PossPron(args[0]))
    case types:
      __no_match__("mkDet",types)

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
  match __types__(args):
    case ["Pron"]:
      return w.PossPron(args[0])
    case types:
      __no_match__("mkQuant",types)

the_Quant = w.DefArt

a_Quant = w.IndefArt

def mkNum(*args):
  match __types__(args):
    case ["Str"]:
      return w.NumCard(w.str2card(args[0]))
    case ["Numeral"]:
      return w.NumCard(w.NumNumeral(args[0]))
    case ["Digits"]:
      return w.NumCard(w.NumDigits(args[0]))
    case ["Digit"]:
      return w.NumCard(w.NumNumeral(w.num(w.pot2as3(w.pot1as2(w.pot0as1(w.pot0(args[0])))))))
    case ["Card"]:
      return w.NumCard(args[0])
    case ["AdN","Card"]:
      return w.NumCard(w.AdNum(args[0],args[1]))
    case types:
      __no_match__("mkNum",types)

singularNum = w.NumSg

pluralNum = w.NumPl

def mkCard(*args):
  match __types__(args):
    case ["Str"]:
      return w.str2card
    case ["Numeral"]:
      return w.NumNumeral(args[0])
    case ["Digits"]:
      return w.NumDigits(args[0])
    case ["AdN","Card"]:
      return w.AdNum(args[0],args[1])
    case types:
      __no_match__("mkCard",types)

def mkOrd(*args):
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
      __no_match__("mkOrd",types)

def mkAdN(*args):
  match __types__(args):
    case ["CAdv"]:
      return w.AdnCAdv(args[0],args[1])
    case types:
      __no_match__("mkAdN",types)

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
  if len(args) == 1 and type(args[0]) is int:
    return w.num(sub1000000000000(args[0]))

  match __types__(args):
    case ["Sub10"]:
      return w.num(w.pot2as3(w.pot1as2(w.pot0as1(args[0]))))
    case ["Sub100"]:
      return w.num(w.pot2as3(w.pot1as2(args[0])))
    case ["Sub1000"]:
      return w.num(w.pot2as3(args[0]))
    case ["Sub1000","Sub1000"]:
      return w.num(w.pot3plus(args[0],args[1]))
    case types:
      __no_match__("mkNumeral",types)

def thousandfoldNumeral(*args):
  match __types__(args):
    case ["Sub1000"]:
      return w.num(w.pot3(args[0]))
    case types:
      __no_match__("thousandfoldNumeral",types)

def mkSub1000(*args):
  match __types__(args):
    case ["Sub100"]:
      return w.pot1as2(args[0])
    case ["Sub10"]:
      return w.pot2(args[0])
    case ["Sub10","Sub100"]:
      return w.pot2plus(args[0],args[1])
    case types:
      __no_match__("mkSub1000",types)

def mkSub100(*args):
  match __types__(args):
    case ["Sub10"]:
      return w.pot0as1(args[0])
    case ["Sub10","Sub10"]:
      return w.pot1plus(args[0],args[1])
    case types:
      __no_match__("mkSub100",types)

def tenfoldSub100(*args):
  match __types__(args):
    case ["Sub10"]:
      return w.pot110(args[0])
    case types:
      __no_match__("tenfoldSub100",types)

def mkDigits(*args):
  if len(args) == 1 and type(args[0]) is int:
    n = args[0]
    expr = pgf.Expr("IDig", [pgf.ExprFun("D_"+str(n % 10))])
    n    = n // 10
    while n != 0:
        expr = pgf.Expr("IIDig", [pgf.ExprFun("D_"+str(n % 10)), expr])
        n    = n // 10
    return expr

  match __types__(args):
    case ["Dig"]:
      return w.IDig(args[0])
    case ["Dig","Digits"]:
      return w.IIDig(args[0],args[1])
    case types:
      __no_match__("mkDigits",types)

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
    case ["Conj","ListCN"]:
      return w.ConjCN(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjCN(args[0],mkListCN(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case types:
      __no_match__("mkCN",types)

def mkAP(*args):
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
      return w.PastPartAP(w.args[0])
    case ["AP","S"]:
      return w.SentAP(args[0],w.EmbedS(args[1]))
    case ["AP","QS"]:
      return w.SentAP(args[0],w.EmbedQS(args[1]))
    case ["AP","VP"]:
      return w.SentAP(args[0],w.EmbedVP(args[1]))
    case ["AP","SC"]:
      return w.SentAP(args[0],args[1])
    case ["AdA","AP"]:
      return w.AdAP(args[0],args[1])
    case ["Conj","AP","AP"]:
      return w.ConjAP(args[0],w.BaseAP(args[1],args[2]))
    case ["Conj","ListAP"]:
      return w.ConjAP(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjAP(args[0],mkListAP(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case ["Ord"]:
      return w.AdjOrd(args[0])
    case ["CAdv","AP","NP"]:
      return w.CAdvAP(args[0],args[1],args[2],args[3])
    case types:
      __no_match__("mkAP",types)

def reflAP(*args):
  match __types__(args):
    case ["A2"]:
      return w.ReflA2(args[0],args[1])
    case types:
      __no_match__("reflAP",types)

def comparAP(*args):
  match __types__(args):
    case ["A"]:
      return w.UseComparA(args[0])
    case types:
      __no_match__("comparAP",types)

def mkAdv(*args):
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
    case ["Conj","ListAdv"]:
      return w.ConjAdv(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjAdv(args[0],mkListAdv(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case types:
      __no_match__("mkAdv",types)

def mkQS(*args):
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
      __no_match__("mkQS",types)

def mkQCl(*args):
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
      return w.QuestVP(args[0],w.ComplV3(args[1],args[2],args[3]))
    case ["IP","VV","VP"]:
      return w.QuestVP(args[0],w.ComplVV(args[1],args[2]))
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
      __no_match__("mkQCl",types)

def mkIComp(*args):
  match __types__(args):
    case ["IAdv"]:
      return w.CompIAdv(args[0])
    case ["IP"]:
      return w.CompIP(args[0])
    case types:
      __no_match__("mkIComp",types)

def mkIP(*args):
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
      __no_match__("mkIP",types)

what_IP = w.whatSg_IP

who_IP = w.whoSg_IP

def mkIAdv(*args):
  match __types__(args):
    case ["Prep","IP"]:
      return w.PrepIP(args[0],args[1])
    case ["IAdv","Adv"]:
      return w.AdvIAdv(args[0],args[1])
    case types:
      __no_match__("mkIAdv",types)

def mkIDet(*args):
  match __types__(args):
    case ["IQuant","Num"]:
      return w.IdetQuant(args[0],args[1])
    case ["IQuant"]:
      return w.IdetQuant(args[0],w.NumSg)
    case types:
      __no_match__("mkIDet",types)

whichPl_IDet = w.IdetQuant(w.which_IQuant,w.NumPl)

whichSg_IDet = w.IdetQuant(w.which_IQuant,w.NumSg)

which_IDet = whichSg_IDet

def mkRS(*args):
  match __types__(args):
    case ["RCl"]:
      return w.TUseRCl(w.TPres,w.ASimul,w.PPos)
    case ["Tense","RCl"]:
      return w.TUseRCl(args[0],w.ASimul,w.PPos)
    case ["Ant","RCl"]:
      return w.TUseRCl(w.TPres,args[0],w.PPos)
    case ["Pol","RCl"]:
      return w.TUseRCl(w.TPres,w.ASimul,args[0])
    case ["Tense","Ant","RCl"]:
      return w.TUseRCl(args[0],args[1],w.PPos)
    case ["Tense","Pol","RCl"]:
      return w.TUseRCl(args[0],w.ASimul,args[1])
    case ["Ant","Pol","RCl"]:
      return w.TUseRCl(w.TPres,args[0],args[1])
    case ["Tense","Ant","Pol","RCl"]:
      return w.TUseRCl
    case ["Temp","Pol","RCl"]:
      return w.UseRCl(args[0],args[1],args[2])
    case ["Conj","RS","RS"]:
      return w.ConjRS(args[0],w.BaseRS(args[1],args[2]))
    case ["Conj","ListRS"]:
      return w.ConjRS(args[0],args[1])
    case ["Conj",list]:
      if args[1]:
          if len(args[1]) > 1:
              return w.ConjRS(args[0],mkListRS(*args[1]))
          else:
              return args[1][0]
      else:
          return None
    case types:
      __no_match__("mkRS",types)

def mkRCl(*args):
  match __types__(args):
    case ["RP","VP"]:
      return w.RelVP(args[0],args[1])
    case ["RP","V"]:
      return w.RelVP(args[0],w.UseV(args[1]))
    case ["RP","V2","NP"]:
      return w.RelVP(args[0],w.ComplV2(args[1],args[2]))
    case ["RP","V3","NP","NP"]:
      return w.RelVP(args[0],w.ComplV3(args[1],args[2],args[3]))
    case ["RP","VV","VP"]:
      return w.RelVP(args[0],w.ComplVV(args[1],args[2]))
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
      __no_match__("mkRCl",types)

which_RP = w.IdRP

def mkRP(*args):
  match __types__(args):
    case ["Prep","NP","RP"]:
      return w.FunRP(args[0],args[1],args[2])
    case types:
      __no_match__("mkRP",types)

def mkSSlash(*args):
  match __types__(args):
    case ["Temp","Pol","ClSlash","SSlash"]:
      return w.UseSlash(args[0],args[1],args[2])
    case types:
      __no_match__("mkSSlash",types)

def mkClSlash(*args):
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
      __no_match__("mkClSlash",types)

def mkVPSlash(*args):
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
    case types:
      __no_match__("mkVPSlash",types)

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
      __no_match__("mkList"+cat,types)

    while i >= 0:
      expr = cons(args[i],expr)
      i = i - 1

    return expr
  else:
    __no_match__("mkList"+cat,types)

mkListS   = mkList
mkListAdv = mkList
mkListAP  = mkList
mkListNP  = mkList
mkListCN  = mkList
mkListRS  = mkList

def mkUttImp(*args):
  match __types__(args):
    case ["ImpForm","Pol","Imp"]:
      return w.case(args[0],w.of)
    case types:
      __no_match__("mkUttImp",types)

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
